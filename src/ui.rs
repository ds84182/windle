use std::{
  cell::{OnceCell, RefCell},
  f32,
  marker::PhantomData,
  rc::Rc,
  sync::Arc,
  time::Duration,
};

use ref_cast::RefCast;
use windows::{
  Foundation::{Size, TimeSpan, TypedEventHandler},
  Graphics::{
    DirectX::{DirectXAlphaMode, DirectXPixelFormat},
    SizeInt32,
  },
  UI::{
    self,
    Composition::{
      ColorKeyFrameAnimation, CompositionColorBrush, CompositionDrawingSurface,
      CompositionSpriteShape, CompositionSurfaceBrush, ContainerVisual, ScalarKeyFrameAnimation,
      ShapeVisual, SpriteVisual, Visual,
    },
    Input::{GestureRecognizer, GestureSettings, PointerPoint},
  },
  Win32::{
    Graphics::{
      Direct2D::{
        Common::{D2D_RECT_F, D2D1_COLOR_F},
        D2D1_ANTIALIAS_MODE_ALIASED, D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT, ID2D1DeviceContext,
      },
      DirectWrite::{
        DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STYLE, DWRITE_FONT_STYLE_NORMAL,
        DWRITE_FONT_WEIGHT, DWRITE_FONT_WEIGHT_BOLD, DWRITE_FONT_WEIGHT_NORMAL,
        DWRITE_TEXT_METRICS,
      },
    },
    System::WinRT::Composition::ICompositionDrawingSurfaceInterop,
  },
  core::{HSTRING, Interface},
};
use windows_numerics::{Vector2, Vector3};

use crate::{
  Game, GameAction, Graphics, GuessWordState, Letter, LetterMap, LetterState, LetterStatus,
  observe::{Observable, Observer},
};

const PROP_ROTATION_ANGLE: &HSTRING = windows::core::h!("RotationAngle");
const PROP_COLOR: &HSTRING = windows::core::h!("Color");

const fn time_span(duration: Duration) -> TimeSpan {
  TimeSpan {
    Duration: duration.as_nanos() as i64 / 100,
  }
}

trait IntoVector2 {
  fn xy(self) -> Vector2;
}

impl IntoVector2 for Vector3 {
  #[inline]
  fn xy(self) -> Vector2 {
    Vector2 {
      X: self.X,
      Y: self.Y,
    }
  }
}

pub(crate) struct UiContext {
  theme: &'static Theme,
  gfx: Rc<Graphics>,
}

impl UiContext {
  pub(crate) fn new(gfx: &Rc<Graphics>) -> Self {
    UiContext {
      theme: DARK,
      gfx: gfx.clone(),
    }
  }
}

#[derive(ref_cast::RefCast)]
#[repr(transparent)]
struct SubClass<M, T>(T, PhantomData<M>);

trait SubClassOf<T>: windows::core::Interface + windows::core::imp::CanInto<T> {}

impl<T, V: windows::core::Interface + windows::core::imp::CanInto<T>> SubClassOf<T> for V {}

type ReturnType<T = ()> = T;

macro_rules! subclass_proxy {
  (
    $class:ty: {
      $(fn $fn_name:ident$([$($generics:tt)*])?($($arg:ident: $arg_ty:ty),* $(,)?) $(-> $ret:ty)?);*
      $(;)?
    }
  ) => {
    impl<T: SubClassOf<$class>> SubClass<$class, T> {
      $(
        #[allow(non_snake_case)]
        fn $fn_name<$($generics)*>(&self, $($arg: $arg_ty,)*) -> windows::core::Result<ReturnType<$($ret)?>> {
          self.0.cast::<$class>()?.$fn_name($($arg)*)
        }
      )*
    }
  };
}

subclass_proxy! {
  Visual: {
    fn Offset() -> Vector3;
    fn RelativeOffsetAdjustment() -> Vector3;

    fn Size() -> Vector2;
    fn RelativeSizeAdjustment() -> Vector2;

    fn AnchorPoint() -> Vector2;
  }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct HitTest {
  size: Size,
  point: Vector2,
}

impl HitTest {
  pub(crate) fn new(size: Size, point: Vector2) -> Self {
    Self { size, point }
  }

  fn for_visual(
    &self,
    visual: &SubClass<Visual, impl SubClassOf<Visual>>,
  ) -> windows::core::Result<HitTest> {
    let size = visual.Size()?;
    let relative_size = visual.RelativeSizeAdjustment()?;
    let size = Size {
      Width: size.X + self.size.Width * relative_size.X,
      Height: size.Y + self.size.Height * relative_size.Y,
    };

    // TODO: Handle rotation, scale, center point, and arbitrary transforms?

    let anchor_point = visual.AnchorPoint()?
      * Vector2 {
        X: size.Width,
        Y: size.Height,
      };

    let offset = (visual.Offset()?.xy()
      + visual.RelativeOffsetAdjustment()?.xy()
        * Vector2 {
          X: self.size.Width,
          Y: self.size.Height,
        })
      - anchor_point;
    let point = self.point - offset;

    Ok(HitTest { size, point })
  }

  fn in_bounds(&self) -> bool {
    !(self.point.X < 0.0
      || self.point.Y < 0.0
      || self.point.X > self.size.Width
      || self.point.Y > self.size.Height)
  }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum Cursor {
  LinkSelect,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum PointerEventKind {
  Down,
  Up,
  Move,
}

pub(crate) trait HitTarget {
  fn cursor(&self) -> Cursor;

  fn on_pointer_event(
    &self,
    action: &Arc<dyn ActionSender<GameAction>>,
    kind: PointerEventKind,
    pointer: &PointerPoint,
  ) -> windows::core::Result<bool>;
}

pub(crate) trait ActionSender<T>: Send + Sync {
  fn send(&self, action: T) -> bool;
}

pub(crate) struct GameUi {
  root: ContainerVisual,
  #[allow(unused)] // Cannot drop (TODO: Can we?)
  backdrop: SpriteVisual,

  #[allow(unused)] // Cannot drop
  mtx: GuessMatrix,
  #[allow(unused)] // Cannot drop
  keyboard: Keyboard,
}

impl GameUi {
  pub(crate) fn new(cx: &Rc<UiContext>, game: &Game) -> windows::core::Result<Self> {
    let root = cx.gfx.comp.CreateContainerVisual()?;
    let backdrop = cx.gfx.comp.CreateSpriteVisual()?;
    backdrop.SetBrush(&cx.gfx.comp.CreateColorBrushWithColor(cx.theme.background)?)?;
    let root_children = root.Children()?;
    root_children.InsertAtBottom(&backdrop)?;

    let mtx = GuessMatrix::new(cx, game.guess_states())?;
    root_children.InsertAtTop(&mtx.visual)?;

    let keyboard = Keyboard::new(cx, &game.keyboard.status)?;
    root_children.InsertAtTop(&keyboard.visual)?;

    root.SetRelativeSizeAdjustment(Vector2 { X: 1.0, Y: 1.0 })?;
    backdrop.SetRelativeSizeAdjustment(Vector2 { X: 1.0, Y: 1.0 })?;

    Ok(Self {
      root,
      backdrop,
      mtx,
      keyboard,
    })
  }

  pub(crate) fn root(&self) -> &ContainerVisual {
    &self.root
  }

  pub(crate) fn hit_test(
    &self,
    hit_test: HitTest,
  ) -> windows::core::Result<Option<Rc<dyn HitTarget>>> {
    let hit_test = hit_test.for_visual(SubClass::ref_cast(&self.keyboard.visual))?;

    if !hit_test.in_bounds() {
      // Keyboard was not hit
      return Ok(None);
    }

    for row in &self.keyboard.rows {
      let hit_test = hit_test.for_visual(SubClass::ref_cast(&row.visual))?;

      if !hit_test.in_bounds() {
        continue;
      }

      for key in &row.keys {
        let hit_test = hit_test.for_visual(SubClass::ref_cast(&key.visual))?;

        if hit_test.in_bounds() {
          return Ok(Some(key.clone()));
        }
      }
    }

    Ok(None)
  }
}

struct Theme {
  background: UI::Color,
  letter_color: UI::Color,
  letter_tile_border_inactive: UI::Color,
  letter_tile_border_active: UI::Color,
  letter_tile_bg_present: UI::Color,
  letter_tile_bg_correct: UI::Color,
  letter_tile_bg_absent: UI::Color,
  key_bg: UI::Color,
  key_bg_present: UI::Color,
  key_bg_correct: UI::Color,
  key_bg_absent: UI::Color,
}

const fn color(color: u32) -> UI::Color {
  let [a, r, g, b] = color.to_be_bytes();
  UI::Color {
    A: a,
    R: r,
    G: g,
    B: b,
  }
}

const DARK: &Theme = &Theme {
  background: color(0xFF121213),
  letter_color: color(0xFFF8F8F8),
  letter_tile_border_inactive: color(0xFF3A3A3C),
  letter_tile_border_active: color(0xFF565758),
  letter_tile_bg_present: color(0xFFB59F3B),
  letter_tile_bg_correct: color(0xFF538D4E),
  letter_tile_bg_absent: color(0xFF3A3A3C),
  key_bg: color(0xFF818384),
  key_bg_present: color(0xFFB59F3B),
  key_bg_correct: color(0xFF538D4E),
  key_bg_absent: color(0xFF3A3A3C),
};

#[derive(Clone, Debug, PartialEq, Eq)]
enum Text {
  Char(char),
  String(HSTRING),
}

impl Text {
  fn as_utf16<'a>(&'a self, buf: &'a mut [u16; 4]) -> &'a [u16] {
    match self {
      Self::Char(char) => char.encode_utf16(buf),
      Self::String(str) => str,
    }
  }
}

#[derive(Copy, Clone, PartialEq)]
struct TextStyle {
  font_size: f32,
  color: UI::Color,
  weight: DWRITE_FONT_WEIGHT,
  style: DWRITE_FONT_STYLE,
}

struct TextSurface {
  text: Text,
  style: TextStyle,
  dirty: bool,
  surface: CompositionDrawingSurface,
  brush: CompositionSurfaceBrush,
  visual: SpriteVisual,
}

impl TextSurface {
  fn new(gfx: &Graphics) -> windows::core::Result<TextSurface> {
    let surface = gfx.comp_gfx_dev.CreateDrawingSurface2(
      SizeInt32 {
        Width: 2,
        Height: 2,
      },
      DirectXPixelFormat::R8G8B8A8UIntNormalized,
      DirectXAlphaMode::Premultiplied,
    )?;
    let brush = gfx.comp.CreateSurfaceBrushWithSurface(&surface)?;
    let visual = gfx.comp.CreateSpriteVisual()?;
    let _ = visual.SetIsPixelSnappingEnabled(true);
    visual.SetBrush(&brush)?;
    Ok(TextSurface {
      text: Text::Char('\0'),
      style: TextStyle {
        font_size: 0.0,
        color: Default::default(),
        weight: DWRITE_FONT_WEIGHT_NORMAL,
        style: DWRITE_FONT_STYLE_NORMAL,
      },
      dirty: false,
      surface,
      brush,
      visual,
    })
  }

  fn update_text(&mut self, new_text: Text) {
    if new_text == self.text {
      return;
    }
    self.text = new_text;
    self.dirty = true;
  }

  fn update_style(&mut self, new_style: TextStyle) {
    if new_style == self.style {
      return;
    }
    self.style = new_style;
    self.dirty = true;
  }

  fn redraw(&mut self, gfx: &Graphics) -> windows::core::Result<bool> {
    if !std::mem::take(&mut self.dirty) {
      return Ok(false);
    }

    let mut buf = [0u16; 4];
    let utf16 = self.text.as_utf16(&mut buf);

    let layout = unsafe {
      let format = gfx.dwrite_factory.CreateTextFormat(
        &gfx.dwrite_ui_font_family,
        &gfx.dwrite_font_collection,
        self.style.weight,
        self.style.style,
        DWRITE_FONT_STRETCH_NORMAL,
        self.style.font_size,
        windows::core::w!("en-US"),
      )?;

      gfx
        .dwrite_factory
        .CreateTextLayout(utf16, &format, f32::MAX, f32::MAX)?
    };

    let mut metrics = DWRITE_TEXT_METRICS::default();
    unsafe {
      layout.GetMetrics(&mut metrics)?;
    }

    let width = metrics.width.ceil() as i32;
    let height = metrics.height.ceil() as i32;

    self.surface.Resize(SizeInt32 {
      Width: width,
      Height: height,
    })?;

    let surface_interop = self.surface.cast::<ICompositionDrawingSurfaceInterop>()?;

    unsafe {
      let mut offset = Default::default();
      let context = surface_interop.BeginDraw::<ID2D1DeviceContext>(None, &mut offset)?;

      context.PushAxisAlignedClip(
        &D2D_RECT_F {
          left: offset.x as f32,
          top: offset.y as f32,
          right: (offset.x + width) as f32,
          bottom: (offset.y + height) as f32,
        },
        D2D1_ANTIALIAS_MODE_ALIASED,
      );

      context.Clear(None);

      let brush = context.CreateSolidColorBrush(
        &D2D1_COLOR_F {
          a: 1.0,
          r: self.style.color.R as f32 / 255.0,
          g: self.style.color.G as f32 / 255.0,
          b: self.style.color.B as f32 / 255.0,
        },
        None,
      )?;

      context.DrawTextLayout(
        Vector2 {
          X: offset.x as f32,
          Y: offset.y as f32,
        },
        &layout,
        &brush,
        D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT,
      );

      context.PopAxisAlignedClip();

      surface_interop.EndDraw()?;
    }

    self.visual.SetSize(Vector2 {
      X: width as f32,
      Y: height as f32,
    })?;

    Ok(true)
  }
}

struct LetterTile {
  cx: Rc<UiContext>,

  visual: ShapeVisual,
  box_shape: CompositionSpriteShape,
  box_fill_brush: CompositionColorBrush,
  box_stroke_brush: CompositionColorBrush,
  char_surface: RefCell<TextSurface>,

  flip_animation: ScalarKeyFrameAnimation,
  color_animation: ColorKeyFrameAnimation,

  observer: OnceCell<Observer<LetterState>>,
}

impl LetterTile {
  const SIZE: f32 = 64.0;
  const FONT_SIZE: f32 = 32.0;
  const COLOR_ANIM_PARAM: &HSTRING = windows::core::h!("color");

  fn new(
    index: u32,
    cx: &Rc<UiContext>,
    state: &Observable<LetterState>,
  ) -> windows::core::Result<Rc<Self>> {
    let gfx = &cx.gfx;

    let visual = gfx.comp.CreateShapeVisual()?;
    let box_geo = gfx.comp.CreateRectangleGeometry()?;
    box_geo.SetSize(Vector2 {
      X: Self::SIZE,
      Y: Self::SIZE,
    })?;
    let box_shape = gfx.comp.CreateSpriteShapeWithGeometry(&box_geo)?;
    let box_fill_brush = gfx.comp.CreateColorBrush()?;
    let box_stroke_brush = gfx.comp.CreateColorBrush()?;
    let char_surface = TextSurface::new(gfx)?;

    visual.Children()?.InsertAtTop(&char_surface.visual)?;
    visual.SetSize(Vector2 {
      X: Self::SIZE,
      Y: Self::SIZE,
    })?;
    visual.Shapes()?.Append(&box_shape)?;
    char_surface.visual.SetRelativeOffsetAdjustment(Vector3 {
      X: 0.5,
      Y: 0.5,
      Z: 0.0,
    })?;

    visual.SetAnchorPoint(Vector2 { X: 0.5, Y: 0.5 })?;
    visual.SetRotationAxis(Vector3 {
      X: 1.0,
      Y: 0.0,
      Z: 0.0,
    })?;

    let flip_animation = gfx.comp.CreateScalarKeyFrameAnimation()?;
    let linear_easing = gfx.comp.CreateLinearEasingFunction()?;
    flip_animation.InsertKeyFrameWithEasingFunction(0.0, 0.0, &linear_easing)?;
    flip_animation.InsertKeyFrameWithEasingFunction(0.5, f32::consts::FRAC_PI_2, &linear_easing)?;
    flip_animation.InsertKeyFrameWithEasingFunction(1.0, 0.0, &linear_easing)?;
    flip_animation.SetDelayTime(time_span(Duration::from_millis(index as u64 * 100)))?;
    flip_animation.SetDuration(time_span(Duration::from_millis(250)))?;

    let color_animation = gfx.comp.CreateColorKeyFrameAnimation()?;
    let step_easing = gfx.comp.CreateStepEasingFunctionWithStepCount(1)?;
    color_animation.InsertKeyFrame(0.0, UI::Color::default())?;
    color_animation.InsertExpressionKeyFrameWithEasingFunction(
      1.0,
      Self::COLOR_ANIM_PARAM,
      &step_easing,
    )?;
    color_animation.SetDelayTime(time_span(Duration::from_millis(index as u64 * 100)))?;
    color_animation.SetDuration(time_span(Duration::from_millis(250)))?;

    // TODO: Spring animation when entire guess is correct. Tiles jump upward and spring downward.

    let this = Rc::new(LetterTile {
      cx: cx.clone(),
      visual,
      box_shape,
      box_fill_brush,
      box_stroke_brush,
      char_surface: RefCell::new(char_surface),
      flip_animation,
      color_animation,
      observer: OnceCell::new(),
    });

    let this_weak = Rc::downgrade(&this);

    let _ = this.observer.set(state.observe(move |_, state| {
      let Some(tile) = this_weak.upgrade() else {
        return;
      };
      if let Err(err) = tile.update(state) {
        eprintln!("Failed to update tile: {err:?}");
      }
    }));

    Ok(this)
  }

  fn set_visual_offset(&self, offset: Vector2) -> windows::core::Result<()> {
    self.visual.SetOffset(Vector3 {
      X: offset.X + Self::SIZE * 0.5,
      Y: offset.Y + Self::SIZE * 0.5,
      Z: 0.0,
    })
  }

  fn set_tile_style(&self, kind: Option<LetterStatus>, active: bool) -> windows::core::Result<()> {
    let cx = &*self.cx;
    // TODO: Use a filter to color the character, then turn the character red when the prefix
    // of a guess cannot be found in the word database.
    match kind {
      None => {
        self.box_shape.SetFillBrush(None)?;
        self.box_shape.SetStrokeBrush(&self.box_stroke_brush)?;
        self.box_shape.SetStrokeThickness(4.0)?;
        self.box_stroke_brush.SetColor(if active {
          cx.theme.letter_tile_border_active
        } else {
          cx.theme.letter_tile_border_inactive
        })?;

        self.box_fill_brush.StopAnimation(PROP_COLOR)?;
        self.visual.StopAnimation(PROP_ROTATION_ANGLE)?;
      }
      Some(kind) => {
        self.box_shape.SetFillBrush(&self.box_fill_brush)?;
        let color = match kind {
          LetterStatus::Correct => cx.theme.letter_tile_bg_correct,
          LetterStatus::Present => cx.theme.letter_tile_bg_present,
          LetterStatus::Absent => cx.theme.letter_tile_bg_absent,
        };
        self.box_shape.SetStrokeBrush(None)?;
        self.box_fill_brush.StopAnimation(PROP_COLOR)?;
        self.box_fill_brush.SetColor(UI::Color::default())?;
        self
          .color_animation
          .SetColorParameter(Self::COLOR_ANIM_PARAM, color)?;
        self
          .box_fill_brush
          .StartAnimation(PROP_COLOR, &self.color_animation)?;
        self
          .visual
          .StartAnimation(PROP_ROTATION_ANGLE, &self.flip_animation)?;
      }
    }
    Ok(())
  }

  fn update(&self, state: &LetterState) -> windows::core::Result<()> {
    let cx = &*self.cx;
    match state {
      LetterState::Empty => {
        self.set_tile_style(None, false)?;
        self.char_surface.borrow().visual.SetBrush(None)?;
      }
      &LetterState::Char {
        letter,
        status: kind,
      } => {
        self.set_tile_style(kind, true)?;

        let mut cs = self.char_surface.borrow_mut();

        cs.update_text(Text::Char(letter.into()));
        cs.update_style(TextStyle {
          font_size: Self::FONT_SIZE,
          color: cx.theme.letter_color,
          weight: DWRITE_FONT_WEIGHT_BOLD,
          style: DWRITE_FONT_STYLE_NORMAL,
        });

        if cs.redraw(&cx.gfx)? {
          let size = cs.surface.SizeInt32()?;
          let (width, height) = (size.Width, size.Height);

          cs.visual.SetOffset(Vector3 {
            X: -(width / 2) as f32,
            Y: -(height / 2) as f32,
            Z: 0.0,
          })?;
        }

        cs.visual.SetBrush(&cs.brush)?;
      }
    }

    Ok(())
  }
}

// impl Elem for Rc<LetterTile> {
//   fn visual(&self) -> Visual {
//     self.visual.cast().unwrap()
//   }

//   fn layout(&self, _cx: &'_ LayoutCx<'_>) -> LayoutSize {
//     LayoutSize {
//       width: LayoutAxis {
//         px: LetterTile::SIZE,
//         pct: 0.0,
//       },
//       height: LayoutAxis {
//         px: LetterTile::SIZE,
//         pct: 0.0,
//       },
//     }
//   }
// }

struct LetterRow {
  visual: ContainerVisual,

  #[allow(unused)] // Cannot drop
  letters: [Rc<LetterTile>; 5],
}

impl LetterRow {
  // Padding between tiles
  const TILE_PAD: f32 = 4.0;

  fn new(
    cx: &Rc<UiContext>,
    letter_states: &[&Observable<LetterState>; 5],
  ) -> windows::core::Result<Self> {
    let visual = cx.gfx.comp.CreateContainerVisual()?;

    let mut letters = [
      LetterTile::new(0, cx, &letter_states[0])?,
      LetterTile::new(1, cx, &letter_states[1])?,
      LetterTile::new(2, cx, &letter_states[2])?,
      LetterTile::new(3, cx, &letter_states[3])?,
      LetterTile::new(4, cx, &letter_states[4])?,
    ];

    let children = visual.Children()?;
    for (i, letter) in letters.iter_mut().enumerate() {
      children.InsertAtTop(&letter.visual)?;
      letter.set_visual_offset(Vector2 {
        X: (i as f32) * (LetterTile::SIZE + Self::TILE_PAD),
        Y: 0.0,
      })?;

      letter.update(&LetterState::Empty)?;
    }

    let width = LetterTile::SIZE + 4.0 * (LetterTile::SIZE + Self::TILE_PAD);
    visual.SetSize(Vector2 {
      X: width,
      Y: LetterTile::SIZE,
    })?;
    visual.SetAnchorPoint(Vector2 { X: 0.5, Y: 0.0 })?;
    visual.SetRelativeOffsetAdjustment(Vector3 {
      X: 0.5,
      Y: 0.0,
      Z: 0.0,
    })?;

    Ok(Self { visual, letters })
  }
}

struct GuessMatrix {
  visual: ContainerVisual,

  #[allow(unused)] // Cannot drop
  rows: [LetterRow; 6],
}

impl GuessMatrix {
  // Padding between tiles
  const ROW_PAD: f32 = 4.0;

  fn new(cx: &Rc<UiContext>, guess_states: &[GuessWordState; 6]) -> windows::core::Result<Self> {
    let visual = cx.gfx.comp.CreateContainerVisual()?;

    let mut rows = [
      LetterRow::new(cx, &guess_states[0].letter_states().each_ref())?,
      LetterRow::new(cx, &guess_states[1].letter_states().each_ref())?,
      LetterRow::new(cx, &guess_states[2].letter_states().each_ref())?,
      LetterRow::new(cx, &guess_states[3].letter_states().each_ref())?,
      LetterRow::new(cx, &guess_states[4].letter_states().each_ref())?,
      LetterRow::new(cx, &guess_states[5].letter_states().each_ref())?,
    ];

    let children = visual.Children()?;
    for (i, row) in rows.iter_mut().enumerate() {
      children.InsertAtTop(&row.visual)?;
      row.visual.SetOffset(Vector3 {
        X: 0.0,
        Y: (i as f32) * (LetterTile::SIZE + Self::ROW_PAD),
        Z: 0.0,
      })?;
    }

    let width = LetterTile::SIZE + 4.0 * (LetterTile::SIZE + LetterRow::TILE_PAD);
    let height = LetterTile::SIZE + 5.0 * (LetterTile::SIZE + Self::ROW_PAD);
    visual.SetSize(Vector2 {
      X: width,
      Y: height,
    })?;
    visual.SetAnchorPoint(Vector2 { X: 0.5, Y: 0.0 })?;
    visual.SetRelativeOffsetAdjustment(Vector3 {
      X: 0.5,
      Y: 0.0,
      Z: 0.0,
    })?;
    visual.SetOffset(Vector3 {
      X: 0.0,
      Y: 8.0,
      Z: 0.0,
    })?;

    Ok(Self { visual, rows })
  }
}

struct Key {
  cx: Rc<UiContext>,

  wide: bool,
  visual: ShapeVisual,
  box_shape: CompositionSpriteShape,
  box_fill_brush: CompositionColorBrush,
  #[allow(unused)] // Cannot drop
  char_surface: RefCell<TextSurface>,

  observer: OnceCell<Observer<Option<LetterStatus>>>,

  gesture_recognizer: RefCell<Option<GestureRecognizer>>,
  game_action: GameAction,
}

impl Key {
  const WIDTH: f32 = 48.0;
  const WIDTH_WIDE: f32 = 64.0;
  const HEIGHT: f32 = 56.0;
  const BORDER_RADIUS: f32 = 4.0;
  const FONT_SIZE: f32 = 20.0;
  const FONT_SIZE_SMALL: f32 = 12.0;

  fn new(
    text: Text,
    wide: bool,
    small_font: bool,
    game_action: GameAction,
    cx: &Rc<UiContext>,
    state: Option<&Observable<Option<LetterStatus>>>,
  ) -> windows::core::Result<Rc<Self>> {
    let gfx = &cx.gfx;

    let width = if wide { Self::WIDTH_WIDE } else { Self::WIDTH };

    let visual = gfx.comp.CreateShapeVisual()?;
    let box_geo = gfx.comp.CreateRoundedRectangleGeometry()?;
    box_geo.SetSize(Vector2 {
      X: width,
      Y: Self::HEIGHT,
    })?;
    box_geo.SetCornerRadius(Vector2 {
      X: Self::BORDER_RADIUS,
      Y: Self::BORDER_RADIUS,
    })?;
    let box_shape = gfx.comp.CreateSpriteShapeWithGeometry(&box_geo)?;
    let box_fill_brush = gfx.comp.CreateColorBrush()?;
    let mut char_surface = TextSurface::new(gfx)?;

    visual.Children()?.InsertAtTop(&char_surface.visual)?;
    visual.SetSize(Vector2 {
      X: width,
      Y: Self::HEIGHT,
    })?;
    visual.Shapes()?.Append(&box_shape)?;
    char_surface.visual.SetRelativeOffsetAdjustment(Vector3 {
      X: 0.5,
      Y: 0.5,
      Z: 0.0,
    })?;

    char_surface.update_text(text);
    char_surface.update_style(TextStyle {
      font_size: if small_font {
        Self::FONT_SIZE_SMALL
      } else {
        Self::FONT_SIZE
      },
      color: cx.theme.letter_color,
      weight: DWRITE_FONT_WEIGHT_BOLD,
      style: DWRITE_FONT_STYLE_NORMAL,
    });

    if char_surface.redraw(&cx.gfx)? {
      let size = char_surface.surface.SizeInt32()?;
      let (width, height) = (size.Width, size.Height);

      char_surface.visual.SetBrush(&char_surface.brush)?;

      char_surface.visual.SetOffset(Vector3 {
        X: -(width / 2) as f32,
        Y: -(height / 2) as f32,
        Z: 0.0,
      })?;
    }

    let this = Rc::new(Self {
      cx: cx.clone(),
      wide,
      visual,
      box_shape,
      box_fill_brush,
      char_surface: RefCell::new(char_surface),
      observer: OnceCell::new(),
      gesture_recognizer: RefCell::default(),
      game_action,
    });

    if let Some(state) = state {
      let this_weak = Rc::downgrade(&this);

      let _ = this.observer.set(state.observe(move |_, state| {
        let Some(key) = this_weak.upgrade() else {
          return;
        };
        if let Err(err) = key.update(*state) {
          eprintln!("Failed to update tile: {err:?}");
        }
      }));
    }

    Ok(this)
  }

  fn size(&self) -> Size {
    Size {
      Width: if self.wide {
        Self::WIDTH_WIDE
      } else {
        Self::WIDTH
      },
      Height: Self::HEIGHT,
    }
  }

  fn update(&self, kind: Option<LetterStatus>) -> windows::core::Result<()> {
    let cx = &*self.cx;
    self.box_shape.SetFillBrush(&self.box_fill_brush)?;
    match kind {
      None => {
        self.box_fill_brush.SetColor(cx.theme.key_bg)?;
      }
      Some(kind) => {
        self.box_fill_brush.SetColor(match kind {
          LetterStatus::Correct => cx.theme.key_bg_correct,
          LetterStatus::Present => cx.theme.key_bg_present,
          LetterStatus::Absent => cx.theme.key_bg_absent,
        })?;
      }
    }
    Ok(())
  }
}

impl HitTarget for Key {
  fn cursor(&self) -> Cursor {
    Cursor::LinkSelect
  }

  fn on_pointer_event(
    &self,
    action: &Arc<dyn ActionSender<GameAction>>,
    kind: PointerEventKind,
    pointer: &PointerPoint,
  ) -> windows::core::Result<bool> {
    let mut gesture_mut = self.gesture_recognizer.borrow_mut();
    let gesture = if let Some(gesture) = &*gesture_mut {
      gesture.clone()
    } else {
      let new_gesture = GestureRecognizer::new()?;
      new_gesture.SetGestureSettings(GestureSettings::Tap)?;
      let action = action.clone();
      let game_action = self.game_action.clone();
      new_gesture.Tapped(&TypedEventHandler::new(move |_, _| {
        action.send(game_action.clone());
        Ok(())
      }))?;
      gesture_mut.insert(new_gesture).clone()
    };
    drop(gesture_mut);

    match kind {
      PointerEventKind::Down => gesture.ProcessDownEvent(pointer)?,
      PointerEventKind::Up => gesture.ProcessUpEvent(pointer)?,
      PointerEventKind::Move => {
        gesture.ProcessMoveEvents(&PointerPoint::GetIntermediatePoints(pointer.PointerId()?)?)?
      }
    }

    Ok(gesture.IsActive()?)
  }
}

struct KeyRow {
  visual: ContainerVisual,

  keys: Box<[Rc<Key>]>,
}

impl KeyRow {
  const PAD: f32 = 6.0;

  fn new(
    cx: &Rc<UiContext>,
    keys: &mut dyn Iterator<Item = windows::core::Result<Rc<Key>>>,
  ) -> windows::core::Result<Self> {
    let visual = cx.gfx.comp.CreateContainerVisual()?;

    let mut keys = keys
      .collect::<windows::core::Result<Vec<_>>>()?
      .into_boxed_slice();

    let children = visual.Children()?;
    let mut x = 0.0;
    for key in keys.iter_mut() {
      children.InsertAtTop(&key.visual)?;
      key.visual.SetOffset(Vector3 {
        X: x,
        Y: 0.0,
        Z: 0.0,
      })?;
      x += Self::PAD + key.size().Width;

      key.update(None)?;
    }

    let this = Self { visual, keys };

    let size = this.size();
    this.visual.SetSize(Vector2 {
      X: size.Width,
      Y: size.Height,
    })?;
    this.visual.SetAnchorPoint(Vector2 { X: 0.5, Y: 0.0 })?;
    this.visual.SetRelativeOffsetAdjustment(Vector3 {
      X: 0.5,
      Y: 0.0,
      Z: 0.0,
    })?;

    Ok(this)
  }

  fn size(&self) -> Size {
    let mut width = Self::PAD * (self.keys.len() - 1) as f32;
    width += self.keys.iter().map(|key| key.size().Width).sum::<f32>();
    Size {
      Width: width,
      Height: Key::HEIGHT,
    }
  }
}

struct Keyboard {
  visual: ContainerVisual,

  rows: [KeyRow; 3],
}

impl Keyboard {
  const PAD: f32 = 8.0;

  fn new(
    cx: &Rc<UiContext>,
    state: &LetterMap<Observable<Option<LetterStatus>>>,
  ) -> windows::core::Result<Self> {
    let gfx = &cx.gfx;

    let visual = gfx.comp.CreateContainerVisual()?;

    let row_1 = KeyRow::new(
      cx,
      &mut b"QWERTYUIOP".iter().map(|x| {
        let letter = Letter::try_from(*x).unwrap();
        Key::new(
          Text::Char(letter.into()),
          false,
          false,
          GameAction::PushLetter(letter),
          cx,
          Some(&state[letter]),
        )
      }),
    )?;

    let row_2 = KeyRow::new(
      cx,
      &mut b"ASDFGHJKL".iter().map(|x| {
        let letter = Letter::try_from(*x).unwrap();
        Key::new(
          Text::Char(letter.into()),
          false,
          false,
          GameAction::PushLetter(letter),
          cx,
          Some(&state[letter]),
        )
      }),
    )?;

    let row_3 = KeyRow::new(
      cx,
      &mut std::iter::once_with(|| {
        Key::new(
          Text::String(windows::core::h!("ENTER").clone()),
          true,
          true,
          GameAction::TryCommitGuess,
          cx,
          None,
        )
      })
      .chain(b"ZXCVBNM".iter().map(|x| {
        let letter = Letter::try_from(*x).unwrap();
        Key::new(
          Text::Char(letter.into()),
          false,
          false,
          GameAction::PushLetter(letter),
          cx,
          Some(&state[letter]),
        )
      }))
      .chain(std::iter::once_with(|| {
        Key::new(
          // TODO: Use Fluent icon font
          Text::String(windows::core::h!("⌫").clone()),
          true,
          false,
          GameAction::PopLetter,
          cx,
          None,
        )
      })),
    )?;

    let this = Self {
      visual,
      rows: [row_1, row_2, row_3],
    };

    let children = this.visual.Children()?;
    let mut y = 0.0;
    for row in this.rows.iter() {
      children.InsertAtTop(&row.visual)?;
      row.visual.SetOffset(Vector3 {
        X: 0.0,
        Y: y,
        Z: 0.0,
      })?;
      y += Self::PAD + row.size().Height;
    }

    let size = this.size();
    this.visual.SetSize(Vector2 {
      X: size.Width,
      Y: size.Height,
    })?;
    this.visual.SetAnchorPoint(Vector2 { X: 0.5, Y: 1.0 })?;
    this.visual.SetRelativeOffsetAdjustment(Vector3 {
      X: 0.5,
      Y: 1.0,
      Z: 0.0,
    })?;
    this.visual.SetOffset(Vector3 {
      X: 0.0,
      Y: -8.0,
      Z: 0.0,
    })?;

    Ok(this)
  }

  fn size(&self) -> Size {
    let width = self
      .rows
      .iter()
      .map(|x| x.size().Width)
      .max_by(f32::total_cmp)
      .unwrap();
    let height = 2.0 * Self::PAD + self.rows.iter().map(|x| x.size().Height).sum::<f32>();

    Size {
      Width: width,
      Height: height,
    }
  }
}
