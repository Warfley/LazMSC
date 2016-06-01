unit Animations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, Math, ExtCtrls, Graphics, Controls, LResources, Dialogs;

type
  PNotifyEvent = ^TNotifyEvent;
  TPaintEvent = procedure(DrawCanvas: TCanvas; Step: integer;
    TimeElapsed: cardinal) of object;
  ECanvasIsNilException = class(Exception);
  EOnPaintIsNilException = class(Exception);

  TAnimationMode = (amNormal, amReverse);

  TAnimation = class(TComponent)
  private
    FSteps, FTime: integer;
    FOnUpdate: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FEnabled: boolean;
    FMode: TAnimationMode;
    procedure SetEnabled(x: boolean);
  protected
    FValue: Pointer;
    LastTime: cardinal;
    LastDelta: cardinal;
    AllTime: cardinal;
    LastStep: integer;
    procedure Update(var Step: integer; const TimeElapsed: cardinal); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; virtual;
    procedure DoUpdate; virtual;
    procedure Stop; virtual;
    property Value: Pointer read FValue write FValue;
  published
    property Steps: integer read FSteps write FSteps;
    property Time: integer read FTime write FTime;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property Enabled: boolean read FEnabled write SetEnabled;
    property Mode: TAnimationMode read FMode write FMode;
  end;

  TTimerAnimation = class(TAnimation)
  private
    Timer: TTimer;
    function getInterval: cardinal;
    function getTime: integer;
    function getSteps: integer;
    procedure setInterval(x: cardinal);
    procedure setTime(x: integer);
    procedure setSteps(x: integer);
    procedure TimerTick(Sender: TObject);
  protected
  public
    procedure Start; override;
    procedure Stop; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Interval: cardinal read getInterval write setInterval;
    property Steps: integer read getSteps write setSteps;
    property Time: integer read getTime write setTime;
    property OnUpdate;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

  TBitmapAnimation = class(TTimerAnimation)
  private
    FOnPaint: TPaintEvent;
    function getBitmap: TBitmap;
    procedure setBitmap(x: TBitmap);
  protected
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    procedure Start; override;
  published
    property Bitmap: TBitmap read getBitmap write setBitmap;
    property Interval;
    property Steps;
    property Time;
    property OnUpdate: TPaintEvent read FOnPaint write FOnPaint;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

  TSpriteBitmapAnimation = class(TBitmapAnimation)
  private
    FSprite: TPicture;
    FRepeatAnimation: boolean;
    FXSprites, FYSprites, FStartValue, FStopValue: cardinal;
    procedure setStartValue(x: cardinal);
    procedure setStopValue(x: cardinal);
  protected
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Sprite: TPicture read FSprite write FSprite;
    property RepeatAnimation: boolean read FRepeatAnimation write FRepeatAnimation;
    property XSprites: cardinal read FXSprites write FXSprites;
    property YSprites: cardinal read FYSprites write FYSprites;
    property StartValue: cardinal read FStartValue write setStartValue;
    property StopValue: cardinal read FStopValue write setStopValue;
    property Bitmap;
    property Interval;
    property Steps;
    property Time;
    property OnUpdate;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;


  TCanvasAnimation = class(TTimerAnimation)
  private
    FPaintEvent: PNotifyEvent;
    FOriginal: TNotifyEvent;
    FCanvas: TCanvas;
    FReleasePaintOnStop: boolean;
    FOnPaint: TPaintEvent;
    procedure setControl(x: TControl);
    function getControl: TControl;
  protected
    procedure Paint(Sender: TObject); virtual;
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Control: TControl read getControl write setControl;
    property PaintEvent: PNotifyEvent read FPaintEvent write FPaintEvent;
  published
    property ReleaseHandleOnStop: boolean read FReleasePaintOnStop
      write FReleasePaintOnStop;
    property OldPaintHandler: TNotifyEvent read FOriginal;
    property Steps;
    property Time;
    property Interval;
    property OnUpdate: TPaintEvent read FOnPaint write FOnPaint;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

  TSpriteCanvasAnimation = class(TCanvasAnimation)
  private
    FSprite: TPicture;
    FXSprites, FYSprites, FStartValue, FEndValue: cardinal;
    FDestRect: TRect;
    FRepeat: boolean;
    bmp: TBitmap;
    procedure SetStartValue(x: cardinal);
    procedure SetStopValue(x: cardinal);
  protected
    procedure Paint(Sender: TObject); override;
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    procedure Start; override;
    procedure Stop; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DestRect: TRect read FDestRect write FDestRect;
  published
    property DoRepeat: boolean read FRepeat write FRepeat;
    property Sprite: TPicture read FSprite write FSprite;
    property XSprites: cardinal read FXSprites write FXSprites;
    property YSprites: cardinal read FYSprites write FYSprites;
    property StartValue: cardinal read FStartValue write SetStartValue;
    property StopValue: cardinal read FEndValue write SetStopValue;
    property Time;
    property Interval;
    property OnUpdate;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

  //Integer Animations

  //32 Bit Integer
  TInteger32TimerAnimation = class(TTimerAnimation)
  private
    FStartValue, FStopValue, FStepValue: integer;
    procedure setStartValue(x: integer);
    procedure setStopValue(x: integer);
    procedure setStepValue(x: integer);
    procedure setValue(x: PInteger);
    function getValue: PInteger;
  protected
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    procedure Start; override;
    constructor Create(AOwner: TComponent); override;
    property Value: PInteger read getValue write setValue;
  published
    property StartValue: integer read FStartValue write setStartValue;
    property StopValue: integer read FStopValue write setStopValue;
    property StepValue: integer read FStepValue write setStepValue;
    property Interval;
    property Steps;
    property Time;
    property OnUpdate;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

  //8Bit Integer
  TInteger8TimerAnimation = class(TTimerAnimation)
  private
    FStartValue, FStopValue, FStepValue: shortint;
    procedure setStartValue(x: shortint);
    procedure setStopValue(x: shortint);
    procedure setStepValue(x: shortint);
    procedure setValue(x: PShortInt);
    function getValue: PShortInt;
  protected
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    procedure Start; override;
    constructor Create(AOwner: TComponent); override;
    property Value: PShortInt read getValue write setValue;
  published
    property StartValue: shortint read FStartValue write setStartValue;
    property StopValue: shortint read FStopValue write setStopValue;
    property StepValue: shortint read FStepValue write setStepValue;
    property Interval;
    property Steps;
    property Time;
    property OnUpdate;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

  //16 Bit Integer
  TInteger16TimerAnimation = class(TTimerAnimation)
  private
    FStartValue, FStopValue, FStepValue: smallint;
    procedure setStartValue(x: smallint);
    procedure setStopValue(x: smallint);
    procedure setStepValue(x: smallint);
    procedure setValue(x: PSmallInt);
    function getValue: PSmallInt;
  protected
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    procedure Start; override;
    constructor Create(AOwner: TComponent); override;
    property Value: PSmallInt read getValue write setValue;
  published
    property StartValue: smallint read FStartValue write setStartValue;
    property StopValue: smallint read FStopValue write setStopValue;
    property StepValue: smallint read FStepValue write setStepValue;
    property Interval;
    property Steps;
    property Time;
    property OnUpdate;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

  //64 Bit Integer
  TInteger64TimerAnimation = class(TTimerAnimation)
  private
    FStartValue, FStopValue, FStepValue: int64;
    procedure setStartValue(x: int64);
    procedure setStopValue(x: int64);
    procedure setStepValue(x: int64);
    procedure setValue(x: PInt64);
    function getValue: PInt64;
  protected
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    procedure Start; override;
    constructor Create(AOwner: TComponent); override;
    property Value: PInt64 read getValue write setValue;
  published
    property StartValue: int64 read FStartValue write setStartValue;
    property StopValue: int64 read FStopValue write setStopValue;
    property StepValue: int64 read FStepValue write setStepValue;
    property Interval;
    property Steps;
    property Time;
    property OnUpdate;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

  //Float Animations

  //Single
  TFloat32TimerAnimation = class(TTimerAnimation)
  private
    FStartValue, FStopValue, FStepValue: single;
    procedure setStartValue(x: single);
    procedure setStopValue(x: single);
    procedure setStepValue(x: single);
    procedure setValue(x: PSingle);
    function getValue: PSingle;
  protected
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    procedure Start; override;
    constructor Create(AOwner: TComponent); override;
    property Value: PSingle read getValue write setValue;
  published
    property StartValue: single read FStartValue write setStartValue;
    property StopValue: single read FStopValue write setStopValue;
    property StepValue: single read FStepValue write setStepValue;
    property Interval;
    property Steps;
    property Time;
    property OnUpdate;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

  //Double
  TFloat64TimerAnimation = class(TTimerAnimation)
  private
    FStartValue, FStopValue, FStepValue: double;
    procedure setStartValue(x: double);
    procedure setStopValue(x: double);
    procedure setStepValue(x: double);
    procedure setValue(x: PDouble);
    function getValue: PDouble;
  protected
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    procedure Start; override;
    constructor Create(AOwner: TComponent); override;
    property Value: PDouble read getValue write setValue;
  published
    property StartValue: double read FStartValue write setStartValue;
    property StopValue: double read FStopValue write setStopValue;
    property StepValue: double read FStepValue write setStepValue;
    property Interval;
    property Steps;
    property Time;
    property OnUpdate;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

  //Extended
  TFloat80TimerAnimation = class(TTimerAnimation)
  private
    FStartValue, FStopValue, FStepValue: extended;
    procedure setStartValue(x: extended);
    procedure setStopValue(x: extended);
    procedure setStepValue(x: extended);
    procedure setValue(x: PExtended);
    function getValue: PExtended;
  protected
    procedure Update(var Step: integer; const TimeElapsed: cardinal); override;
  public
    procedure Start; override;
    constructor Create(AOwner: TComponent); override;
    property Value: PExtended read getValue write setValue;
  published
    property StartValue: extended read FStartValue write setStartValue;
    property StopValue: extended read FStopValue write setStopValue;
    property StepValue: extended read FStepValue write setStepValue;
    property Interval;
    property Steps;
    property Time;
    property OnUpdate;
    property OnStart;
    property OnStop;
    property Enabled;
    property Mode;
  end;

const
  tmNone = -1;
  spNone = -1;

procedure Register;

implementation

procedure Register;
begin
  {$I AnimationRessources.lrs}
  RegisterComponents('Animations', [TInteger32TimerAnimation,
    TInteger8TimerAnimation, TInteger16TimerAnimation, TInteger64TimerAnimation,
    TFloat32TimerAnimation, TFloat64TimerAnimation, TFloat80TimerAnimation,
    TCanvasAnimation, TSpriteCanvasAnimation, TBitmapAnimation, TSpriteBitmapAnimation]);
end;


{ TSpriteBitmapAnimation }


procedure TSpriteBitmapAnimation.setStartValue(x: cardinal);
begin
  if x <= FStopValue then
  begin
    FStartValue := x;
    Steps := (FStopValue - FStartValue);
  end;
end;

procedure TSpriteBitmapAnimation.setStopValue(x: cardinal);
begin
  if x >= FStartValue then
  begin
    FStopValue := x;
    Steps := (FStopValue - FStartValue);
  end;
end;

procedure TSpriteBitmapAnimation.Update(var Step: integer; const TimeElapsed: cardinal);
var
  bmp: TBitmap;
  wd, hg, x, y: integer;
begin
  bmp := TBitmap.Create;
  try
    bmp.Assign(FSprite.Graphic);
    wd := bmp.Width div FXSprites;
    hg := bmp.Height div FYSprites;
    x := ((LastStep + FStartValue) mod FXSprites) * wd;
    y := ((LastStep div FYSprites + FStartValue)) * hg;
    Bitmap.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), bmp.Canvas, Rect(x, y, x + wd, y + hg));
    inherited;
  finally
    bmp.Free;
  end;
end;

constructor TSpriteBitmapAnimation.Create(AOwner: TComponent);
begin

end;

destructor TSpriteBitmapAnimation.Destroy;
begin

end;



{ TBitmapAnimation }


function TBitmapAnimation.getBitmap: TBitmap;
begin
  Result := TBitmap(FValue);
end;

procedure TBitmapAnimation.setBitmap(x: TBitmap);
begin
  FValue := x;
end;

procedure TBitmapAnimation.Update(var Step: integer; const TimeElapsed: cardinal);
begin
  if Assigned(FOnPaint) and Assigned(Bitmap) then
    FOnPaint(Bitmap.Canvas, Step, TimeElapsed);
end;

procedure TBitmapAnimation.Start;
begin
  if Assigned(Bitmap) then
    inherited;
end;

{ TSpriteCanvasAnimation }

procedure TSpriteCanvasAnimation.SetStartValue(x: cardinal);
begin
  if x <= FEndValue then
  begin
    FStartValue := x;
    Steps := (FEndValue - FStartValue);
  end;
end;

procedure TSpriteCanvasAnimation.Update(var Step: integer; const TimeElapsed: cardinal);
begin
  inherited;
  if (Step = Steps) and FRepeat then
    Step := 0;
end;

procedure TSpriteCanvasAnimation.SetStopValue(x: cardinal);
begin
  if x >= FStartValue then
  begin
    FEndValue := Min(FXSprites * FYSprites - 1, x);
    Steps := (FEndValue - FStartValue);
  end;
end;

procedure TSpriteCanvasAnimation.Paint(Sender: TObject);
var
  wd, hg, x, y: integer;
begin
  wd := bmp.Width div FXSprites;
  hg := bmp.Height div FYSprites;
  x := ((LastStep + FStartValue) mod FXSprites) * wd;
  y := ((LastStep div FYSprites + FStartValue)) * hg;
  FCanvas.CopyRect(FDestRect, bmp.Canvas, Rect(x, y, x + wd, y + hg));
  inherited;
end;

procedure TSpriteCanvasAnimation.Start;
begin
  inherited;
  bmp.Assign(FSprite.Graphic);
end;

procedure TSpriteCanvasAnimation.Stop;
begin
  inherited;
  bmp.Width := 0;
  bmp.Height := 0;
end;

constructor TSpriteCanvasAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FSprite := TPicture.Create;
  bmp := TBitmap.Create;
  DestRect := Rect(0, 0, 0, 0);
  FStartValue := 0;
  FEndValue := 0;
  Steps := 0;
end;

destructor TSpriteCanvasAnimation.Destroy;
begin
  inherited;
  bmp.Free;
  FSprite.Free;
end;

{ TCanvasAnimation }


constructor TCanvasAnimation.Create(AOwner: TComponent);
begin
  inherited;
  ReleaseHandleOnStop := False;
end;

destructor TCanvasAnimation.Destroy;
begin
  Stop;
  inherited;
end;

procedure TCanvasAnimation.setControl(x: TControl);
begin
  Value := x;
end;

function TCanvasAnimation.getControl: TControl;
begin
  Result := TControl(Value);
end;

procedure TCanvasAnimation.Paint(Sender: TObject);
begin
  if Assigned(OnUpdate) then
    OnUpdate(FCanvas, LastStep, 0);
  if Assigned(FOriginal) then
    FOriginal(Control);
end;

procedure TCanvasAnimation.Update(var Step: integer; const TimeElapsed: cardinal);
begin
  if FCanvas = nil then
  begin
    Stop;
    raise ECanvasIsNilException.Create('No Canvas found');
  end;
  if FPaintEvent = nil then
  begin
    Stop;
    raise EOnPaintIsNilException.Create('No Paint Property found');
  end;
  LastStep := Step;
  LastDelta := TimeElapsed;
  if Assigned(Control) then
    Control.Repaint;
end;

procedure TCanvasAnimation.Start;
begin
  if FEnabled then
    Stop;
  if FCanvas = nil then
  begin
    Stop;
    raise ECanvasIsNilException.Create('No Canvas found');
  end;
  if FPaintEvent = nil then
  begin
    Stop;
    raise EOnPaintIsNilException.Create('No Paint Property found');
  end;
  if FPaintEvent^ <> @Paint then
  begin
    FOriginal := FPaintEvent^;
    FPaintEvent^ := @Paint;

  end;
  inherited;
end;

procedure TCanvasAnimation.Stop;
begin
  if ReleaseHandleOnStop then
    if Assigned(FPaintEvent) then
      FPaintEvent^ := FOriginal;
  inherited;
end;


{ TFloat80TimerAnimation }


procedure TFloat80TimerAnimation.Start;
begin
  if assigned(FValue) then
  begin
    if Mode = amNormal then
      PExtended(FValue)^ := StartValue
    else
      PExtended(FValue)^ := StopValue;
    inherited;
  end;
end;

procedure TFloat80TimerAnimation.setStartValue(x: extended);
begin
  if x <> FStopValue then
  begin
    Steps := round(abs((StartValue - StopValue) / StepValue));
    FStartValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;

  end;
end;

procedure TFloat80TimerAnimation.setStopValue(x: extended);
begin
  if x <> FStartValue then
  begin
    Steps := round(abs((StartValue - StopValue) / StepValue));
    FStopValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TFloat80TimerAnimation.setStepValue(x: extended);
begin
  if x <> 0 then
  begin
    FStepValue := x;
    Steps := round(abs((StartValue - StopValue) / StepValue));
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TFloat80TimerAnimation.setValue(x: PExtended);
begin
  FValue := x;
end;

function TFloat80TimerAnimation.getValue: PExtended;
begin
  Result := PExtended(FValue);
end;

procedure TFloat80TimerAnimation.Update(var Step: integer; const TimeElapsed: cardinal);
begin
  if Mode = amNormal then
    PExtended(FValue)^ := PExtended(FValue)^ + FStepValue
  else
    PExtended(FValue)^ := PExtended(FValue)^ - FStepValue;

end;

constructor TFloat80TimerAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartValue := 0;
  FStopValue := 1;
  FStepValue := 0.1;
  Steps := 10;
  Interval := 100;
end;

{ TFloat64TimerAnimation }


procedure TFloat64TimerAnimation.Start;
begin
  if assigned(FValue) then
  begin
    if Mode = amNormal then
      PDouble(FValue)^ := StartValue
    else
      PDouble(FValue)^ := StopValue;
    inherited;
  end;
end;

procedure TFloat64TimerAnimation.setStartValue(x: double);
begin
  if x <> FStopValue then
  begin
    Steps := round(abs((StartValue - StopValue) / StepValue));
    FStartValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;

  end;
end;

procedure TFloat64TimerAnimation.setStopValue(x: double);
begin
  if x <> FStartValue then
  begin
    Steps := round(abs((StartValue - StopValue) / StepValue));
    FStopValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TFloat64TimerAnimation.setStepValue(x: double);
begin
  if x <> 0 then
  begin
    FStepValue := x;
    Steps := round(abs((StartValue - StopValue) / StepValue));
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TFloat64TimerAnimation.setValue(x: PDouble);
begin
  FValue := x;
end;

function TFloat64TimerAnimation.getValue: PDouble;
begin
  Result := PDouble(FValue);
end;

procedure TFloat64TimerAnimation.Update(var Step: integer; const TimeElapsed: cardinal);
begin
  if Mode = amNormal then
    PDouble(FValue)^ := PDouble(FValue)^ + FStepValue
  else
    PDouble(FValue)^ := PDouble(FValue)^ - FStepValue;

end;

constructor TFloat64TimerAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartValue := 0;
  FStopValue := 1;
  FStepValue := 0.1;
  Steps := 10;
  Interval := 100;
end;



{ TFloat32TimerAnimation }


procedure TFloat32TimerAnimation.Start;
begin
  if assigned(FValue) then
  begin
    if Mode = amNormal then
      PSingle(FValue)^ := StartValue
    else
      PSingle(FValue)^ := StopValue;
    inherited;
  end;
end;

procedure TFloat32TimerAnimation.setStartValue(x: single);
begin
  if x <> FStopValue then
  begin
    Steps := round(abs((StartValue - StopValue) / StepValue));
    FStartValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;

  end;
end;

procedure TFloat32TimerAnimation.setStopValue(x: single);
begin
  if x <> FStartValue then
  begin
    Steps := round(abs((StartValue - StopValue) / StepValue));
    FStopValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TFloat32TimerAnimation.setStepValue(x: single);
begin
  if x <> 0 then
  begin
    FStepValue := x;
    Steps := round(abs((StartValue - StopValue) / StepValue));
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TFloat32TimerAnimation.setValue(x: PSingle);
begin
  FValue := x;
end;

function TFloat32TimerAnimation.getValue: PSingle;
begin
  Result := PSingle(FValue);
end;

procedure TFloat32TimerAnimation.Update(var Step: integer; const TimeElapsed: cardinal);
begin
  if Mode = amNormal then
    PSingle(FValue)^ := PSingle(FValue)^ + FStepValue
  else
    PSingle(FValue)^ := PSingle(FValue)^ - FStepValue;

end;

constructor TFloat32TimerAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartValue := 0;
  FStopValue := 1;
  FStepValue := 0.1;
  Steps := 10;
  Interval := 100;
end;

{ TInteger64TimerAnimation }


procedure TInteger64TimerAnimation.Start;
begin
  if assigned(FValue) then
  begin
    if Mode = amNormal then
      PInt64(FValue)^ := StartValue
    else
      PInt64(FValue)^ := StopValue;
    inherited;
  end;
end;

procedure TInteger64TimerAnimation.setStartValue(x: int64);
begin
  if x <> FStopValue then
  begin
    Steps := abs((StartValue - StopValue) div StepValue);
    FStartValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;

  end;
end;

procedure TInteger64TimerAnimation.setStopValue(x: int64);
begin
  if x <> FStartValue then
  begin
    Steps := abs((StartValue - StopValue) div StepValue);
    FStopValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TInteger64TimerAnimation.setStepValue(x: int64);
begin
  if x <> 0 then
  begin
    FStepValue := x;
    Steps := abs((StartValue - StopValue) div StepValue);
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TInteger64TimerAnimation.setValue(x: PInt64);
begin
  FValue := x;
end;

function TInteger64TimerAnimation.getValue: PInt64;
begin
  Result := PInt64(FValue);
end;

procedure TInteger64TimerAnimation.Update(var Step: integer;
  const TimeElapsed: cardinal);
begin
  if Mode = amNormal then
    PInt64(FValue)^ := PInt64(FValue)^ + FStepValue
  else
    PInt64(FValue)^ := PInt64(FValue)^ - FStepValue;

end;

constructor TInteger64TimerAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartValue := 0;
  FStopValue := 10;
  FStepValue := 1;
  Steps := 10;
  Interval := 100;
end;

{ TInteger16TimerAnimation }


procedure TInteger16TimerAnimation.Start;
begin
  if assigned(FValue) then
  begin
    if Mode = amNormal then
      PSmallInt(FValue)^ := StartValue
    else
      PSmallInt(FValue)^ := StopValue;
    inherited;
  end;
end;

procedure TInteger16TimerAnimation.setStartValue(x: smallint);
begin
  if x <> FStopValue then
  begin
    Steps := abs((StartValue - StopValue) div StepValue);
    FStartValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;

  end;
end;

procedure TInteger16TimerAnimation.setStopValue(x: smallint);
begin
  if x <> FStartValue then
  begin
    Steps := abs((StartValue - StopValue) div StepValue);
    FStopValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TInteger16TimerAnimation.setStepValue(x: smallint);
begin
  if x <> 0 then
  begin
    FStepValue := x;
    Steps := abs((StartValue - StopValue) div StepValue);
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TInteger16TimerAnimation.setValue(x: PSmallInt);
begin
  FValue := x;
end;

function TInteger16TimerAnimation.getValue: PSmallInt;
begin
  Result := PSmallInt(FValue);
end;

procedure TInteger16TimerAnimation.Update(var Step: integer;
  const TimeElapsed: cardinal);
begin
  if Mode = amNormal then
    PSmallInt(FValue)^ := PSmallInt(FValue)^ + FStepValue
  else
    PSmallInt(FValue)^ := PSmallInt(FValue)^ - FStepValue;

end;

constructor TInteger16TimerAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartValue := 0;
  FStopValue := 10;
  FStepValue := 1;
  Steps := 10;
  Interval := 100;
end;

{ TInteger8TimerAnimation }


procedure TInteger8TimerAnimation.Start;
begin
  if assigned(FValue) then
  begin
    if Mode = amNormal then
      PShortInt(FValue)^ := StartValue
    else
      PShortInt(FValue)^ := StopValue;
    inherited;
  end;
end;

procedure TInteger8TimerAnimation.setStartValue(x: shortint);
begin
  if x <> FStopValue then
  begin
    Steps := abs((StartValue - StopValue) div StepValue);
    FStartValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;

  end;
end;

procedure TInteger8TimerAnimation.setStopValue(x: shortint);
begin
  if x <> FStartValue then
  begin
    Steps := abs((StartValue - StopValue) div StepValue);
    FStopValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TInteger8TimerAnimation.setStepValue(x: shortint);
begin
  if x <> 0 then
  begin
    FStepValue := x;
    Steps := abs((StartValue - StopValue) div StepValue);
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TInteger8TimerAnimation.setValue(x: PShortInt);
begin
  FValue := x;
end;

function TInteger8TimerAnimation.getValue: PShortInt;
begin
  Result := PShortInt(FValue);
end;

procedure TInteger8TimerAnimation.Update(var Step: integer; const TimeElapsed: cardinal);
begin
  if Mode = amNormal then
    PShortInt(FValue)^ := PShortInt(FValue)^ + FStepValue
  else
    PShortInt(FValue)^ := PShortInt(FValue)^ - FStepValue;

end;

constructor TInteger8TimerAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartValue := 0;
  FStopValue := 10;
  FStepValue := 1;
  Steps := 10;
  Interval := 100;
end;


{ TInteger32TimerAnimation }


procedure TInteger32TimerAnimation.Start;
begin
  if assigned(FValue) then
  begin
    if Mode = amNormal then
      PInteger(FValue)^ := StartValue
    else
      PInteger(FValue)^ := StopValue;
    inherited;
  end;
end;

procedure TInteger32TimerAnimation.setStartValue(x: integer);
begin
  if x <> FStopValue then
  begin
    Steps := abs((StartValue - StopValue) div StepValue);
    FStartValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;

  end;
end;

procedure TInteger32TimerAnimation.setStopValue(x: integer);
begin
  if x <> FStartValue then
  begin
    Steps := abs((StartValue - StopValue) div StepValue);
    FStopValue := x;
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TInteger32TimerAnimation.setStepValue(x: integer);
begin
  if x <> 0 then
  begin
    FStepValue := x;
    Steps := abs((StartValue - StopValue) div StepValue);
    if ((StartValue > StopValue) and (StepValue > 0)) or
      ((StartValue < StopValue) and (StepValue < 0)) then
      FStepValue := -FStepValue;
  end;
end;

procedure TInteger32TimerAnimation.setValue(x: PInteger);
begin
  FValue := x;
end;

function TInteger32TimerAnimation.getValue: PInteger;
begin
  Result := PInteger(FValue);
end;

procedure TInteger32TimerAnimation.Update(var Step: integer;
  const TimeElapsed: cardinal);
begin
  if Mode = amNormal then
    PInteger(FValue)^ := PInteger(FValue)^ + FStepValue
  else
    PInteger(FValue)^ := PInteger(FValue)^ - FStepValue;

end;

constructor TInteger32TimerAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartValue := 0;
  FStopValue := 10;
  FStepValue := 1;
  Steps := 10;
  Interval := 100;
end;


{ TTimerAnimation }


procedure TTimerAnimation.Start;
begin
  Timer.Enabled := True;
  inherited;
end;

procedure TTimerAnimation.Stop;
begin
  Timer.Enabled := False;
  inherited;
end;

procedure TTimerAnimation.TimerTick(Sender: TObject);
begin
  DoUpdate;
end;

function TTimerAnimation.getInterval: cardinal;
begin
  Result := Timer.Interval;
end;

function TTimerAnimation.getTime: integer;
begin
  if Steps = tmNone then
    Result := spNone
  else
    Result := Interval * Steps;
end;

function TTimerAnimation.getSteps: integer;
begin
  Result := inherited Steps;
end;

procedure TTimerAnimation.setInterval(x: cardinal);
begin
  Timer.Interval := x;
end;

procedure TTimerAnimation.setTime(x: integer);
begin
  Timer.Interval := x div Steps;
end;

procedure TTimerAnimation.setSteps(x: integer);
begin
  inherited Steps := x;
end;

constructor TTimerAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Timer := TTimer.Create(nil);
  Timer.Enabled := False;
  Timer.OnTimer := @TimerTick;
  inherited Time := tmNone;
  inherited Steps := 10;
  setInterval(100);
end;

destructor TTimerAnimation.Destroy;
begin
  Timer.Free;
end;



{ TAnimation }

constructor TAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FMode := amNormal;
  FEnabled := False;
  FTime := tmNone;
  FSteps := spNone;
end;

procedure TAnimation.SetEnabled(x: boolean);
begin
  if not x then
    Stop
  else
    Start;
end;

procedure TAnimation.Start;
begin
  FEnabled := True;
  AllTime := 0;
  LastTime := GetTickCount;
  LastStep := -1;

  if assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TAnimation.DoUpdate;
var
  t: cardinal;
  st: integer;
begin
  if FEnabled and assigned(FValue) then
  begin
    t := GetTickCount - LastTime;
    Inc(AllTime, t);
    if (FSteps = spNone) or (FTime = tmNone) then
      st := LastStep + 1
    else
      st := Min(FSteps, round((AllTime) / FTime * FSteps));
    if st > LastStep then
    begin
      Update(st, t);
      if assigned(FOnUpdate) then
        FOnUpdate(Self);
    end;
    Inc(LastTime, t);
    LastStep := st;
    LastDelta := t;
    if FTime > tmNone then
      if AllTime >= FTime then
        Stop;
    if FSteps > spNone then
      if st >= FSteps then
        Stop;
  end;
end;

procedure TAnimation.Stop;
begin
  if FEnabled then
  begin
    FEnabled := False;
    if assigned(FOnStop) then
      FOnStop(Self);

  end;
end;

end.
