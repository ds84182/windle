use std::{
  cell::{Cell, OnceCell},
  collections::BTreeMap,
  rc::Rc,
  sync::Arc,
};

use windows::{
  Devices::Input::PointerDeviceType,
  Foundation::Size,
  UI::{
    Composition::{CompositionGraphicsDevice, Compositor, Desktop::DesktopWindowTarget},
    Input::PointerPoint,
  },
  Win32::{
    Foundation::{HMODULE, HWND},
    Graphics::{
      Direct2D::{
        D2D1_CREATION_PROPERTIES, D2D1_DEBUG_LEVEL_NONE,
        D2D1_DEVICE_CONTEXT_OPTIONS_ENABLE_MULTITHREADED_OPTIMIZATIONS,
        D2D1_THREADING_MODE_SINGLE_THREADED, D2D1CreateDevice, ID2D1Device, ID2D1Factory,
      },
      Direct3D::{
        D3D_DRIVER_TYPE_UNKNOWN, D3D_FEATURE_LEVEL_9_1, D3D_FEATURE_LEVEL_9_2,
        D3D_FEATURE_LEVEL_9_3, D3D_FEATURE_LEVEL_10_0, D3D_FEATURE_LEVEL_10_1,
        D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_11_1,
      },
      Direct3D11::{
        D3D11_CREATE_DEVICE_BGRA_SUPPORT, D3D11_SDK_VERSION, D3D11CreateDevice, ID3D11Device,
        ID3D11DeviceContext,
      },
      DirectWrite::{self, DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory, IDWriteFontCollection},
      Dxgi::{self, DXGI_GPU_PREFERENCE_MINIMUM_POWER, IDXGIAdapter, IDXGIDevice, IDXGIFactory6},
    },
    System::WinRT::{
      Composition::{ICompositorDesktopInterop, ICompositorInterop},
      CreateDispatcherQueueController, DQTAT_COM_STA, DQTYPE_THREAD_CURRENT,
      DispatcherQueueOptions,
    },
    UI::{
      Input::Pointer::EnableMouseInPointer,
      WindowsAndMessaging::{
        MSG, NONCLIENTMETRICSW, SPI_GETNONCLIENTMETRICS, SYSTEM_PARAMETERS_INFO_UPDATE_FLAGS,
        SystemParametersInfoW, WM_POINTERACTIVATE, WM_POINTERCAPTURECHANGED,
        WM_POINTERDEVICECHANGE, WM_POINTERDEVICEINRANGE, WM_POINTERDEVICEOUTOFRANGE,
        WM_POINTERDOWN, WM_POINTERENTER, WM_POINTERHWHEEL, WM_POINTERLEAVE, WM_POINTERUP,
        WM_POINTERUPDATE, WM_POINTERWHEEL,
      },
    },
  },
  core::{HSTRING, Interface},
};
use windows_numerics::Vector2;
use winit::platform::windows::{EventLoopBuilderExtWindows, WindowAttributesExtWindows};

use crate::observe::Observable;

mod observe;
mod ui;

fn main() {
  unsafe {
    EnableMouseInPointer(true).expect("Could not enable mouse in pointer mode");
  }

  let _dqc = unsafe {
    CreateDispatcherQueueController(DispatcherQueueOptions {
      dwSize: std::mem::size_of::<DispatcherQueueOptions>() as u32,
      threadType: DQTYPE_THREAD_CURRENT,
      apartmentType: DQTAT_COM_STA,
    })
    .unwrap()
  };

  let gfx = Graphics::init().unwrap();

  let pointer_id = Rc::new(Cell::new(0u16));
  let event_loop = winit::event_loop::EventLoop::<GameAction>::with_user_event()
    .with_msg_hook({
      let pointer_id = pointer_id.clone();
      move |msg| {
        let msg = msg as *const MSG;
        let msg = unsafe { &*msg };

        match msg.message {
          WM_POINTERACTIVATE
          | WM_POINTERCAPTURECHANGED
          | WM_POINTERDEVICECHANGE
          | WM_POINTERDEVICEINRANGE
          | WM_POINTERDEVICEOUTOFRANGE
          | WM_POINTERDOWN
          | WM_POINTERENTER
          | WM_POINTERLEAVE
          | WM_POINTERUP
          | WM_POINTERUPDATE
          | WM_POINTERWHEEL
          | WM_POINTERHWHEEL => {
            pointer_id.set(msg.wParam.0 as u16);

            false
          }
          _ => false,
        }
      }
    })
    .build()
    .unwrap();
  event_loop.set_control_flow(winit::event_loop::ControlFlow::Wait);
  let proxy = Arc::new(event_loop.create_proxy());
  event_loop
    .run_app(&mut App {
      window: None,
      gfx: Rc::new(gfx),
      game_ui: None,
      game: Game::new(),
      input: InputState {
        ctrl: false,
        shift: false,
        alt: false,
        pointer_id,
        current_target: OnceCell::new(),
      },
      proxy,
    })
    .unwrap();
}

struct InputState {
  ctrl: bool,
  shift: bool,
  alt: bool,
  pointer_id: Rc<Cell<u16>>,
  current_target: OnceCell<Rc<dyn ui::HitTarget>>,
}

impl ui::ActionSender<GameAction> for winit::event_loop::EventLoopProxy<GameAction> {
  fn send(&self, action: GameAction) -> bool {
    self.send_event(action).is_ok()
  }
}

struct App {
  window: Option<Window>,
  gfx: Rc<Graphics>,
  game_ui: Option<ui::GameUi>,
  game: Game,
  input: InputState,
  proxy: Arc<winit::event_loop::EventLoopProxy<GameAction>>,
}

impl winit::application::ApplicationHandler<GameAction> for App {
  fn resumed(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
    let window = self
      .window
      .insert(Window::init(&self.gfx, event_loop).unwrap());
    let game_ui = self
      .game_ui
      .insert(ui::GameUi::new(&Rc::new(ui::UiContext::new(&self.gfx)), &self.game).unwrap());

    self.game.reset_random(WordSet::Duotrigordle);

    window.comp_target.SetRoot(game_ui.root()).unwrap();
  }

  fn window_event(
    &mut self,
    event_loop: &winit::event_loop::ActiveEventLoop,
    _: winit::window::WindowId,
    event: winit::event::WindowEvent,
  ) {
    match event {
      winit::event::WindowEvent::Touch(input) => {
        // Because we enabled mouse in pointer mode, mouse events come in as touch events.
        // We can then use the Pointer ID from the window message with PointerPoint.
        // This allows us to use GestureRecognizer.

        let event = match input.phase {
          winit::event::TouchPhase::Started => ui::PointerEventKind::Down,
          winit::event::TouchPhase::Ended => ui::PointerEventKind::Up,
          winit::event::TouchPhase::Cancelled => ui::PointerEventKind::Up,
          winit::event::TouchPhase::Moved => ui::PointerEventKind::Move,
        };

        let pointer_point =
          PointerPoint::GetCurrentPoint(self.input.pointer_id.get() as u32).unwrap();
        let point = pointer_point.Position().unwrap();

        let device = pointer_point.PointerDevice().unwrap();
        let mouse = device.PointerDeviceType().unwrap() == PointerDeviceType::Mouse;

        let Some(ui) = &self.game_ui else { return };

        let window = self.window.as_ref().unwrap().winit_window.as_ref().unwrap();
        let logical_window_size = window.inner_size().to_logical(window.scale_factor());

        let target = self.input.current_target.get().cloned().or_else(|| {
          ui.hit_test(ui::HitTest::new(
            Size {
              Width: logical_window_size.width,
              Height: logical_window_size.height,
            },
            Vector2 {
              X: if mouse { point.X.round() } else { point.X },
              Y: if mouse { point.Y.round() } else { point.Y },
            },
          ))
          .unwrap()
        });

        let Some(target) = target else {
          window.set_cursor(winit::window::CursorIcon::Default);

          return;
        };

        window.set_cursor(match target.cursor() {
          ui::Cursor::LinkSelect => winit::window::CursorIcon::Pointer,
        });

        let grab_focus = target
          .on_pointer_event(
            &(self.proxy.clone() as Arc<dyn ui::ActionSender<GameAction>>),
            event,
            &pointer_point,
          )
          .expect("Failed to send pointer event to target");

        if grab_focus {
          let _ = self.input.current_target.set(target);
        } else {
          let _ = self.input.current_target.take();
        }
      }
      winit::event::WindowEvent::KeyboardInput {
        event,
        is_synthetic: false,
        ..
      } => {
        if let winit::keyboard::Key::Named(named) = &event.logical_key {
          match named {
            winit::keyboard::NamedKey::Control => {
              self.input.ctrl = event.state.is_pressed();
            }
            winit::keyboard::NamedKey::Shift => {
              self.input.shift = event.state.is_pressed();
            }
            winit::keyboard::NamedKey::Alt => {
              self.input.alt = event.state.is_pressed();
            }
            // Backspace handled here to allow key repeat:
            winit::keyboard::NamedKey::Backspace if event.state.is_pressed() => {
              self.game.pop_letter();
            }
            _ => {}
          }
        }

        if event.repeat || event.state == winit::event::ElementState::Released {
          return;
        }

        if event.logical_key == winit::keyboard::Key::Named(winit::keyboard::NamedKey::Enter) {
          let _ = self.game.try_commit_guess(); // TODO: Handle commit error
        } else if self.input.ctrl
          && event.logical_key
            == winit::keyboard::Key::Character(winit::keyboard::SmolStr::new("r"))
        {
          // Ctrl-R pressed, restart game.
          self.game.reset_random(WordSet::Duotrigordle);
        } else if !self.input.ctrl && !self.input.alt {
          if let Some(text) = event.text {
            for char in text.chars() {
              if let Ok(letter) = Letter::try_from(char) {
                self.game.push_letter(letter);
              }
            }
          }
        }
      }
      winit::event::WindowEvent::CloseRequested => {
        // Destroy the window.
        self.window.as_mut().unwrap().winit_window = None;
      }
      winit::event::WindowEvent::Destroyed => {
        event_loop.exit();
      }
      _ => {}
    }
  }

  fn user_event(&mut self, _event_loop: &winit::event_loop::ActiveEventLoop, event: GameAction) {
      match event {
        GameAction::PopLetter => {
          self.game.pop_letter();
        },
        GameAction::PushLetter(letter) => {
          self.game.push_letter(letter);
        }
        GameAction::TryCommitGuess => {
          let _ = self.game.try_commit_guess();
        }
      }
  }
}

#[allow(unused)]
struct Graphics {
  comp: Compositor,
  comp_interop: ICompositorInterop,
  comp_gfx_dev: CompositionGraphicsDevice,

  d3d11_dev: ID3D11Device,
  d3d11_ctx: ID3D11DeviceContext,

  d2d_dev: ID2D1Device,
  d2d_factory: ID2D1Factory,

  dwrite_factory: IDWriteFactory,
  dwrite_font_collection: IDWriteFontCollection,
  dwrite_ui_font_family: HSTRING,
}

impl Graphics {
  fn init() -> windows::core::Result<Self> {
    let comp = Compositor::new()?;

    // Initialize D3D11 context on integrated graphics.
    let dxgi_factory = unsafe { Dxgi::CreateDXGIFactory1::<IDXGIFactory6>()? };
    let adapter = unsafe {
      dxgi_factory
        .EnumAdapterByGpuPreference::<IDXGIAdapter>(0, DXGI_GPU_PREFERENCE_MINIMUM_POWER)?
    };

    let (d3d11_dev, d3d11_ctx) = unsafe {
      let mut device = None;
      let mut feature_level = Default::default();
      let mut imm_ctx = None;

      D3D11CreateDevice(
        Some(&adapter),
        D3D_DRIVER_TYPE_UNKNOWN,
        HMODULE::default(),
        D3D11_CREATE_DEVICE_BGRA_SUPPORT,
        Some(&[
          D3D_FEATURE_LEVEL_11_1,
          D3D_FEATURE_LEVEL_11_0,
          D3D_FEATURE_LEVEL_10_1,
          D3D_FEATURE_LEVEL_10_0,
          D3D_FEATURE_LEVEL_9_3,
          D3D_FEATURE_LEVEL_9_2,
          D3D_FEATURE_LEVEL_9_1,
        ]),
        D3D11_SDK_VERSION,
        Some(&mut device),
        Some(&mut feature_level),
        Some(&mut imm_ctx),
      )?;

      (device.unwrap(), imm_ctx.unwrap())
    };

    let d2d_dev = unsafe {
      D2D1CreateDevice(
        &d3d11_dev.cast::<IDXGIDevice>()?,
        Some(&D2D1_CREATION_PROPERTIES {
          threadingMode: D2D1_THREADING_MODE_SINGLE_THREADED,
          debugLevel: D2D1_DEBUG_LEVEL_NONE,
          options: D2D1_DEVICE_CONTEXT_OPTIONS_ENABLE_MULTITHREADED_OPTIMIZATIONS,
        }),
      )?
    };

    let d2d_factory = unsafe { d2d_dev.GetFactory()? };

    let dwrite_factory =
      unsafe { DirectWrite::DWriteCreateFactory::<IDWriteFactory>(DWRITE_FACTORY_TYPE_SHARED)? };

    let ui_font_family_name = unsafe {
      let mut metrics = NONCLIENTMETRICSW::default();
      metrics.cbSize = size_of::<NONCLIENTMETRICSW>() as u32;
      let _ = SystemParametersInfoW(
        SPI_GETNONCLIENTMETRICS,
        size_of::<NONCLIENTMETRICSW>() as u32,
        Some(std::ptr::from_mut(&mut metrics).cast()),
        SYSTEM_PARAMETERS_INFO_UPDATE_FLAGS::default(),
      )?;

      windows::core::PCWSTR::from_raw(metrics.lfMessageFont.lfFaceName.as_ptr()).to_hstring()
    };

    let dwrite_font_collection = unsafe {
      let mut font_collection = None;
      dwrite_factory.GetSystemFontCollection(&mut font_collection, false)?;
      font_collection.unwrap()
    };

    let _dwrite_ui_font_family = unsafe {
      let mut index = 0;
      let mut exists = false.into();
      dwrite_font_collection.FindFamilyName(&ui_font_family_name, &mut index, &mut exists)?;

      if !exists.as_bool() {
        panic!("Could not find system font");
      }

      dwrite_font_collection.GetFontFamily(index)?
    };

    let comp_interop = comp.cast::<ICompositorInterop>().unwrap();
    let comp_gfx_dev = unsafe { comp_interop.CreateGraphicsDevice(&d2d_dev).unwrap() };

    Ok(Self {
      comp,
      comp_interop,
      comp_gfx_dev,
      d3d11_dev,
      d3d11_ctx,
      d2d_dev,
      d2d_factory,
      dwrite_factory,
      dwrite_font_collection,
      dwrite_ui_font_family: ui_font_family_name,
    })
  }
}

struct Window {
  // Set to `None` when the window is closed.
  winit_window: Option<winit::window::Window>,
  comp_target: DesktopWindowTarget,
}

impl Window {
  const MIN_SIZE: winit::dpi::PhysicalSize<u32> = winit::dpi::PhysicalSize::new(560, 640);

  fn init(
    graphics: &Graphics,
    event_loop: &winit::event_loop::ActiveEventLoop,
  ) -> anyhow::Result<Self> {
    let winit_window = event_loop.create_window(
      winit::window::WindowAttributes::default()
        .with_no_redirection_bitmap(true)
        .with_title("Windle")
        .with_min_inner_size(Self::MIN_SIZE)
        .with_inner_size(Self::MIN_SIZE),
    )?;

    let interop = graphics.comp.cast::<ICompositorDesktopInterop>()?;

    use winit::raw_window_handle::{HasWindowHandle, RawWindowHandle};
    let RawWindowHandle::Win32(hwnd) = winit_window.window_handle().unwrap().as_raw() else {
      panic!()
    };
    let comp_target =
      unsafe { interop.CreateDesktopWindowTarget(HWND(hwnd.hwnd.get() as _), true)? };

    Ok(Self {
      winit_window: Some(winit_window),
      comp_target,
    })
  }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
enum Letter {
  A = b'A',
  B = b'B',
  C = b'C',
  D = b'D',
  E = b'E',
  F = b'F',
  G = b'G',
  H = b'H',
  I = b'I',
  J = b'J',
  K = b'K',
  L = b'L',
  M = b'M',
  N = b'N',
  O = b'O',
  P = b'P',
  Q = b'Q',
  R = b'R',
  S = b'S',
  T = b'T',
  U = b'U',
  V = b'V',
  W = b'W',
  X = b'X',
  Y = b'Y',
  Z = b'Z',
}

impl std::fmt::Debug for Letter {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    std::fmt::Display::fmt(self, f)
  }
}

impl std::fmt::Display for Letter {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use std::fmt::Write;
    f.write_char((*self).into())
  }
}

impl From<Letter> for char {
  fn from(value: Letter) -> Self {
    value as u8 as char
  }
}

impl TryFrom<u8> for Letter {
  type Error = ();
  fn try_from(value: u8) -> Result<Self, Self::Error> {
    Ok(match value {
      b'A' => Self::A,
      b'B' => Self::B,
      b'C' => Self::C,
      b'D' => Self::D,
      b'E' => Self::E,
      b'F' => Self::F,
      b'G' => Self::G,
      b'H' => Self::H,
      b'I' => Self::I,
      b'J' => Self::J,
      b'K' => Self::K,
      b'L' => Self::L,
      b'M' => Self::M,
      b'N' => Self::N,
      b'O' => Self::O,
      b'P' => Self::P,
      b'Q' => Self::Q,
      b'R' => Self::R,
      b'S' => Self::S,
      b'T' => Self::T,
      b'U' => Self::U,
      b'V' => Self::V,
      b'W' => Self::W,
      b'X' => Self::X,
      b'Y' => Self::Y,
      b'Z' => Self::Z,
      _ => return Err(()),
    })
  }
}

impl TryFrom<char> for Letter {
  type Error = ();
  fn try_from(value: char) -> Result<Self, Self::Error> {
    if value.is_ascii_alphabetic() {
      Letter::try_from(value.to_ascii_uppercase() as u8)
    } else {
      Err(())
    }
  }
}

impl Letter {
  fn try_from_index(index: u8) -> Option<Letter> {
    if index < 26 {
      // SAFETY: Enum discriminants cover the checked range.
      Some(unsafe { std::mem::transmute(index + Letter::A as u8) })
    } else {
      None
    }
  }

  fn index(self) -> u8 {
    (self as u8) - (Letter::A as u8)
  }
}

// TODO: Manual debug impl? { A: <value>, B: <value>, ... }
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct LetterMap<T>([T; 26]);

impl<T> From<[T; 26]> for LetterMap<T> {
  fn from(value: [T; 26]) -> Self {
    LetterMap(value)
  }
}

impl<T> LetterMap<T> {
  const fn new(value: T) -> Self
  where
    T: Copy,
  {
    LetterMap([value; 26])
  }

  fn from_fn(mut f: impl FnMut(Letter) -> T) -> Self {
    Self(std::array::from_fn(|i| {
      f(Letter::try_from_index(i as u8).unwrap())
    }))
  }

  const fn from_array(array: [T; 26]) -> Self {
    Self(array)
  }

  fn iter_mut(&mut self) -> impl Iterator<Item = (Letter, &mut T)> {
    self
      .0
      .iter_mut()
      .enumerate()
      .map(|(i, item)| (Letter::try_from_index(i as u8).unwrap(), item))
  }
}

impl<T> std::ops::Index<Letter> for LetterMap<T> {
  type Output = T;
  fn index(&self, letter: Letter) -> &Self::Output {
    &self.0[letter.index() as usize]
  }
}

impl<T> std::ops::IndexMut<Letter> for LetterMap<T> {
  fn index_mut(&mut self, letter: Letter) -> &mut Self::Output {
    &mut self.0[letter.index() as usize]
  }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Word([Letter; 5]);

impl std::fmt::Debug for Word {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    std::fmt::Display::fmt(self, f)
  }
}

impl std::fmt::Display for Word {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use std::fmt::Write;
    for letter in self.0 {
      f.write_char(letter.into())?;
    }
    Ok(())
  }
}

impl TryFrom<[u8; 5]> for Word {
  type Error = ();
  fn try_from(value: [u8; 5]) -> Result<Self, Self::Error> {
    let mut word = Word([Letter::A; 5]);
    for (byte, letter) in value.iter().zip(word.letters_mut()) {
      *letter = Letter::try_from(*byte)?;
    }
    Ok(word)
  }
}

impl Word {
  fn letters(&self) -> &[Letter; 5] {
    &self.0
  }

  fn letters_mut(&mut self) -> &mut [Letter; 5] {
    &mut self.0
  }
}

struct WordDB {
  /// List of all valid words.
  list: Vec<Word>,
  /// Map from word to word index. It is a `BTreeMap` so words can be queried by prefix.
  index_map: BTreeMap<Word, usize>,
  /// Index of words in the NYT word set.
  nyt_word_set: Vec<u32>,
  /// Index of words in the Duotrigordle word set.
  duotrigordle_word_set: Vec<u32>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum WordSet {
  All,
  NYT,
  Duotrigordle,
}

impl WordDB {
  // TODO: We can process this in a const fn? Alternatively add zstd compression.
  const WORDS: &[u8] = include_bytes!("../words-valid.txt");
  const NYT_WORDS: &[u8] = include_bytes!("../words-nyt.txt");
  const DUOTRIGORDLE_WORDS: &[u8] = include_bytes!("../words-duotrigordle.txt");

  fn iter_words(word_list: &[u8]) -> impl Iterator<Item = Word> {
    word_list.split(|x| *x == b'\n').flat_map(|word| {
      if word.is_empty() {
        return None;
      }

      Some(
        Word::try_from(TryInto::<[u8; 5]>::try_into(word).expect("Word is not 5 characters long"))
          .expect("Invalid word"),
      )
    })
  }

  fn load() -> Self {
    let words = Self::iter_words(Self::WORDS).peekable();
    let mut nyt_words = Self::iter_words(Self::NYT_WORDS).peekable();
    let mut duotrigordle_words = Self::iter_words(Self::DUOTRIGORDLE_WORDS).peekable();

    // Words are stored in alphabetical order in the word lists.
    // We iterate through words, pushing them into the word list. When a word matches the peeked
    // NYT or Duotrigordle word, we update the respective word set.

    let mut list = Vec::with_capacity(14855);
    let mut index_map = BTreeMap::new();
    let mut nyt_word_set = Vec::with_capacity(2314);
    let mut duotrigordle_word_set = Vec::with_capacity(2654);

    for word in words {
      let index = list.len();
      list.push(word);
      index_map.insert(word, index);
      if nyt_words.peek() == Some(&word) {
        nyt_word_set.push(index as u32);
        nyt_words.next();
      }
      if duotrigordle_words.peek() == Some(&word) {
        duotrigordle_word_set.push(index as u32);
        duotrigordle_words.next();
      }
    }

    assert_eq!(nyt_words.next(), None);
    assert_eq!(duotrigordle_words.next(), None);

    Self {
      list,
      index_map,
      nyt_word_set,
      duotrigordle_word_set,
    }
  }

  fn word_count(&self, set: WordSet) -> usize {
    match set {
      WordSet::All => self.list.len(),
      WordSet::NYT => self.nyt_word_set.len(),
      WordSet::Duotrigordle => self.duotrigordle_word_set.len(),
    }
  }

  fn word(&self, index: usize) -> Word {
    self.list[index]
  }

  fn find(&self, word: Word) -> Option<usize> {
    self.index_map.get(&word).copied()
  }

  fn pick_word(&self, set: WordSet, index: usize) -> usize {
    match set {
      WordSet::All => index,
      WordSet::NYT => self.nyt_word_set[index] as usize,
      WordSet::Duotrigordle => self.duotrigordle_word_set[index] as usize,
    }
  }
}

#[derive(Copy, Clone)]
enum LetterState {
  Empty,
  Char {
    letter: Letter,
    status: Option<LetterStatus>,
  },
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum LetterStatus {
  Correct,
  Present,
  Absent,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum GuessStatus {
  Guessing,
  Guessed { correct: bool },
}

struct GuessWordState {
  status: Observable<GuessStatus>,
  letters: [Observable<LetterState>; 5],
}

impl GuessWordState {
  const fn new() -> Self {
    Self {
      status: Observable::new(GuessStatus::Guessing),
      letters: [const { Observable::new(LetterState::Empty) }; 5],
    }
  }

  fn letter_states(&self) -> &[Observable<LetterState>; 5] {
    &self.letters
  }

  fn is_committed(&self) -> bool {
    matches!(self.status.get(), GuessStatus::Guessed { .. })
  }

  fn reset(&mut self) {
    self.status.set_if_changed(GuessStatus::Guessing);
    self.letters.iter_mut().for_each(|letter| {
      if let LetterState::Empty = letter.get() {
        return;
      }
      letter.set(LetterState::Empty);
    });
  }

  fn partial_word(&self) -> (Word, usize) {
    let mut word = Word([Letter::A; 5]);
    for (i, (state, out)) in self.letters.iter().zip(word.letters_mut()).enumerate() {
      match state.get() {
        LetterState::Empty => return (word, i),
        LetterState::Char { letter, .. } => {
          *out = *letter;
        }
      }
    }
    (word, 5)
  }

  fn push(&mut self, letter: Letter) -> bool {
    for state in &mut self.letters {
      let LetterState::Empty = state.get() else {
        continue;
      };
      state.set(LetterState::Char {
        letter,
        status: None,
      });
      return true;
    }
    false
  }

  fn pop(&mut self) -> bool {
    if self.is_committed() {
      return false;
    }
    for state in self.letters.iter_mut().rev() {
      if let LetterState::Empty = state.get() {
        continue;
      }
      state.set(LetterState::Empty);
      return true;
    }
    false
  }

  fn try_commit(&mut self, current_word: Word, word_db: &WordDB) -> Result<(), GuessCommitError> {
    assert!(!self.is_committed());
    let (guess_word, 5) = self.partial_word() else {
      // Entire word not guessed
      return Err(GuessCommitError::NotEnoughLetters);
    };
    if word_db.find(guess_word).is_none() {
      return Err(GuessCommitError::InvalidWord);
    }
    let mismatched = [
      current_word.letters()[0] != guess_word.letters()[0],
      current_word.letters()[1] != guess_word.letters()[1],
      current_word.letters()[2] != guess_word.letters()[2],
      current_word.letters()[3] != guess_word.letters()[3],
      current_word.letters()[4] != guess_word.letters()[4],
    ];
    for (index, (state, letter)) in self
      .letters
      .iter_mut()
      .zip(current_word.letters())
      .enumerate()
    {
      let LetterState::Char { letter: guess, .. } = state.get() else {
        panic!()
      };

      let kind = Some(if guess == letter {
        LetterStatus::Correct
      } else {
        // Count all mismatched occurrences of guess in word
        let mismatch_char_count = mismatched
          .iter()
          .zip(current_word.letters())
          .filter(|(mismatch, mismatch_letter)| **mismatch && *mismatch_letter == guess)
          .count();

        // Count all mismatched occurrences of guess letter in guess before current index
        let prev_guess_count = guess_word
          .letters()
          .iter()
          .zip(mismatched.iter())
          .take(index + 1)
          .filter(|(prev, mismatched)| **mismatched && **prev == *guess)
          .count();

        if prev_guess_count <= mismatch_char_count {
          LetterStatus::Present
        } else {
          LetterStatus::Absent
        }
      });

      state.set(LetterState::Char {
        letter: *guess,
        status: kind,
      });
    }
    self.status.set(GuessStatus::Guessed {
      correct: mismatched == [false; 5],
    });
    Ok(())
  }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum GuessCommitError {
  NotEnoughLetters,
  InvalidWord,
  GameOver,
}

struct Keyboard {
  status: LetterMap<Observable<Option<LetterStatus>>>,
}

impl Keyboard {
  const fn new() -> Self {
    Self {
      status: const { LetterMap([const { Observable::new(None) }; 26]) },
    }
  }

  fn reset(&mut self) {
    for (_, status) in self.status.iter_mut() {
      status.set_if_changed(None);
    }
  }

  fn update(&mut self, word: Word, guesses: &[Word]) {
    let mut mismatched_char_counts = LetterMap::new(0u8);
    // Filter out word characters that have matches in a guessed word.
    for (i, &letter) in word.letters().iter().enumerate() {
      if !guesses.iter().any(|guess| guess.letters()[i] == letter) {
        mismatched_char_counts[letter] += 1;
      }
    }

    for (letter, status) in self.status.iter_mut() {
      let mut new_status = None;

      // Abuse short circuiting to run the clauses like a Prolog predicate
      let _ = Self::key_predicate_clause_grey(letter, word, guesses, &mut new_status)
        || Self::key_predicate_clause_green(letter, word, &mismatched_char_counts, &mut new_status)
        || Self::key_predicate_clause_green_gaslight(letter, word, guesses, &mut new_status)
        || Self::key_predicate_clause_yellow(letter, word, guesses, &mut new_status)
        || Self::key_predicate_clause_unused(&mut new_status);

      status.set_if_changed(new_status);
    }
  }

  // Predicates ported from Prolog, don't mind the mess.

  /// Letter is grey if it is not part of the word and is used in a guess.
  fn key_predicate_clause_grey(
    letter: Letter,
    word: Word,
    guesses: &[Word],
    status: &mut Option<LetterStatus>,
  ) -> bool {
    *status = Some(LetterStatus::Absent);
    !word.letters().contains(&letter)
      && guesses
        .iter()
        .any(|guess| guess.letters().contains(&letter))
  }

  /// Word letter is green if there are no mismatched occurrences.
  fn key_predicate_clause_green(
    letter: Letter,
    word: Word,
    mismatched_char_counts: &LetterMap<u8>,
    status: &mut Option<LetterStatus>,
  ) -> bool {
    *status = Some(LetterStatus::Correct);
    word.letters().contains(&letter) && mismatched_char_counts[letter] == 0
  }

  /// Keyboard should gaslight player into thinking all occurrences have been found if the times the
  /// letter is used in guesses is equal to the correctly matched occurrences.
  fn key_predicate_clause_green_gaslight(
    letter: Letter,
    word: Word,
    guesses: &[Word],
    status: &mut Option<LetterStatus>,
  ) -> bool {
    *status = Some(LetterStatus::Correct);

    let found_in_word;

    word.letters().contains(&letter)
      // Count how many occurrences of the letter have been successfully matched for all guesses.
      && {
        found_in_word = word.letters()
          .iter()
          .enumerate()
          .filter(|&(j, &l)| l == letter && guesses.iter().any(|guess| guess.letters()[j] == l))
          .count();
        found_in_word > 0
      }
      // Find the max number of occurrences of the letter across all guesses.
      && guesses.iter()
        .map(|guess| guess.letters().iter().filter(|&&l| l == letter).count())
        .max()
        .unwrap_or(0) == found_in_word
  }

  // Word letter is yellow if there are still mismatched occurrences AND the letter has been
  // colored yellow in a guess.
  // A letter is colored yellow when the index of the char occurrence is <= the number of
  // mismatched occurences between the word and the guess. This can be simplified to the number of
  // mismatches in a guess for the letter being > 0.
  fn key_predicate_clause_yellow(
    letter: Letter,
    word: Word,
    guesses: &[Word],
    status: &mut Option<LetterStatus>,
  ) -> bool {
    *status = Some(LetterStatus::Present);

    word.letters().contains(&letter)
      // Are there any words where the letter is colored yellow?
      && guesses.iter().any(|guess| {
        // Count the number of mismatches for this letter in the guess, and check if its greater
        // than zero.
        guess
          .letters()
          .iter()
          .enumerate()
          .filter(|&(i, &l)| l == letter && word.letters()[i] != letter)
          .count()
          > 0
      })
  }

  /// Otherwise char has not been used yet.
  fn key_predicate_clause_unused(status: &mut Option<LetterStatus>) -> bool {
    *status = None;
    true
  }
}

#[derive(Clone, Debug)]
enum GameAction {
  PushLetter(Letter),
  PopLetter,
  TryCommitGuess,
}

struct Game {
  word_db: WordDB,
  current_word: Word,
  guesses: [GuessWordState; 6],
  keyboard: Keyboard,
}

impl Game {
  fn new() -> Self {
    Game {
      word_db: WordDB::load(),
      current_word: Word([Letter::A; 5]),
      guesses: [const { GuessWordState::new() }; 6],
      keyboard: const { Keyboard::new() },
    }
  }

  fn guess_states(&self) -> &[GuessWordState; 6] {
    &self.guesses
  }

  /// Reset the game with the given word as the target word.
  fn reset(&mut self, current_word: Word) {
    self.current_word = current_word;
    for guess in &mut self.guesses {
      guess.reset();
    }
    self.keyboard.reset();
  }

  /// Reset with a random word from the given word set.
  fn reset_random(&mut self, set: WordSet) {
    self.reset(
      self.word_db.word(
        self
          .word_db
          .pick_word(set, rand::random_range(0..self.word_db.word_count(set))),
      ),
    )
  }

  /// Attempt to push a letter into the latest guess word.
  fn push_letter(&mut self, letter: Letter) -> bool {
    for guess in self.guesses.iter_mut() {
      if guess.is_committed() {
        continue;
      }
      return guess.push(letter);
    }
    false
  }

  /// Attempt to pop a letter from the latest guess word.
  fn pop_letter(&mut self) -> bool {
    for guess in self.guesses.iter_mut() {
      if guess.pop() {
        return true;
      }
    }
    false
  }

  /// Attempt to commit the latest guess word, returning an error if the commit fails.
  fn try_commit_guess(&mut self) -> Result<(), GuessCommitError> {
    for guess in self.guesses.iter_mut() {
      if guess.is_committed() {
        continue;
      }
      guess.try_commit(self.current_word, &self.word_db)?;

      // Update keyboard state
      let words_iter = self
        .guesses
        .iter()
        .map(|guess| guess.partial_word())
        .take_while(|(_, count)| *count == 5)
        .map(|(word, _)| word);
      let mut guess_words = [Word([Letter::A; 5]); 6];
      let mut guess_word_count = 0;
      for word in words_iter {
        guess_words[guess_word_count] = word;
        guess_word_count += 1;
      }

      self
        .keyboard
        .update(self.current_word, &guess_words[..guess_word_count]);

      return Ok(());
    }
    Err(GuessCommitError::GameOver)
  }
}

/*

winit initialization (done)
Init Composition interop (done)
Init Direct3D and Direct2D (done)
Init DirectWrite (done)

Initial layout (64x64px max squares containing word characters, repeated 6 times) (done)
Keyboard layout (done)
Game state struct (done)
Input processing (done)
Move core game code to game.rs
Game loop
Animation (using DComp animations)

*/
