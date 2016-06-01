unit MinimalTrackBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Math, DrawUtils, GraphUtil;

type

  TTrackButton = (tbNone, tbAuto, tbButton);

  TMinimalTrackBar = class(TGraphicControl)
  private
    FLast: integer;
    FOnChange: TNotifyEvent;
    FButtonType: TTrackButton;
    FGrow: boolean;
    FButtonColor: TColor;
    FMouseDown: boolean;
    FMouseOver: boolean;
    FMin, FMax, FPosition: integer;
    FButton3D: boolean;
    procedure SetPosition(x: integer);
    procedure SetMin(x: integer);
    procedure SetMax(x: integer);
    procedure SetButtonColor(x: TColor);
    procedure SetButtonType(x: TTrackButton);
    procedure SetButton3D(x: boolean);
    { Private declarations }
  protected
    procedure Change;
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: integer; Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X: integer; Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X: integer; Y: integer); override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  published
    property Button3D: boolean read FButton3D write SetButton3D;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ButtonType: TTrackButton read FButtonType write SetButtonType;
    property ButtonColor: TColor read FButtonColor write SetButtonColor;
    property Grow: boolean read FGrow write FGrow;
    property MousePressed: boolean read FMouseDown;
    property MouseOver: boolean read FMouseOver;
    property Position: integer read FPosition write SetPosition;
    property Max: integer read FMax write SetMax;
    property Min: integer read FMin write SetMin;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

procedure Register;

implementation

procedure TMinimalTrackBar.SetButton3D(x: boolean);
begin
  FButton3D := x;
  Invalidate;
end;

procedure TMinimalTrackBar.MouseEnter;
begin
  FMouseOver := True;
  inherited MouseEnter;
  Invalidate;
end;

procedure TMinimalTrackBar.MouseLeave;
begin
  FMouseOver := False;
  inherited MouseLeave;
  Invalidate;
end;

procedure TMinimalTrackBar.Resize;
begin
  Self.Height := Math.Max(Height, 15);
  Self.Height := Math.Min(Height, Width div 3);
  Invalidate;
  inherited Resize;
end;

procedure TMinimalTrackBar.SetPosition(x: integer);
begin
  if (x >= FMin) and (x <= FMax) then
    FPosition := x;
  if (FLast >= FMin) and (FLast <> FPosition) then
    Change;
  FLast := FPosition;
end;

procedure TMinimalTrackBar.SetMin(x: integer);
begin
  if FMax - FMin > 0 then
    FMin := x;
  FLast := FMin - 1;
  Invalidate;
end;

procedure TMinimalTrackBar.SetMax(x: integer);
begin
  if FMax - FMin > 0 then
    FMax := x;
  FLast := FMin - 1;
  Invalidate;
end;

procedure TMinimalTrackBar.SetButtonColor(x: TColor);
begin
  FButtonColor := x;
  Invalidate;
end;

procedure TMinimalTrackBar.SetButtonType(x: TTrackButton);
begin
  FButtonType := x;
  Invalidate;
end;

procedure TMinimalTrackBar.Change;
begin
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TMinimalTrackBar.Paint;
var
  DrawBtn: boolean;
  BarSize: integer;
  r, diff: integer;
begin
  case FButtonType of
    tbNone:
    begin
      if FGrow then
      begin
        if FMouseOver then
          BarSize := Height
        else
          BarSize := Height div 2;
      end
      else
        BarSize := Height;
      DrawBtn := False;
    end;
    tbAuto:
    begin
      if FGrow then
      begin
        if FMouseOver then
          BarSize := (Height div 2)
        else
          BarSize := Height div 3;
      end
      else
        BarSize := (Height div 2);
      DrawBtn := FMouseOver;
    end;
    tbButton:
    begin
      if FGrow then
      begin
        if FMouseOver then
          BarSize := Height div 2
        else
          BarSize := Height div 3;
      end
      else
        BarSize := Height div 2;
      DrawBtn := True;
    end;
  end;


  r := 5;
  if BarSize <= 8 then
    r := BarSize;
  diff := Self.Height - BarSize;
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Color := Math.Max(Math.Min(BlackNWhite(Self.Color), clSilver), $005F5F5F);
    Pen.Color := DarkenColor(Brush.Color, 20);
    RoundRect(Self.Height div 2, diff div 2, Self.Width - (Self.Height div 2),
      Self.Height - diff div 2, r, r);
    if Enabled then
      if MouseOver then
        Brush.Color := LightenColor(Self.Color, 20)
      else
        Brush.Color := Self.Color;
    Pen.Style := psClear;
    RoundRect(Self.Height div 2, diff div 2, Round(
      (FPosition - FMin) / (FMax - FMin) * (Self.Width - Self.Height) +
      Self.Height div 2),
      Self.Height - diff div 2, r, r);
  end;
  if DrawBtn and Enabled then
  begin
    if Button3D then
    begin
      if not FMouseDown then
      begin
        r := round(Height / 100 * 15) div 2;
        DrawGradientCircle(Canvas,
          Rect(round((FPosition - FMin) / (FMax - FMin) * (Width - Height)),
          0, round((FPosition - FMin) / (FMax - FMin) * (Width - Height)) +
          (Height), Height),
          GetHighLightColor(ButtonColor),
          GetShadowColor(ButtonColor));
        if (r > 0) then
          DrawGradientCircle(Canvas,
            Rect(round(r + (FPosition - FMin) / (FMax - FMin) * (Width - Height)),
            r, round((FPosition - FMin) / (FMax - FMin) * (Width - Height)) -
            r + (Height), Height - r),
            (ButtonColor),
            GetHighLightColor(ButtonColor));

      end
      else
      begin
        r := (Self.Height div 2) div 3 * 2;
        DrawGradientCircle(Canvas,
          Rect(round((FPosition - FMin) / (FMax - FMin) * (Width - Height)),
          0, round((FPosition - FMin) / (FMax - FMin) * (Width - Height)) +
          (Height), Height),
          GetHighLightColor(ButtonColor),
          GetShadowColor(ButtonColor));
        if (r > 0) then
        begin
          Canvas.Brush.Color := GetHighLightColor(ButtonColor);
          Canvas.Ellipse(round((FPosition - FMin) / (FMax - FMin) * (Width - Height)) +
            Height div 4, Height div 4, round((FPosition - FMin) /
            (FMax - FMin) * (Width - Height)) + (Height - Height div 4),
            Height - Height div 4);
        end;
      end;
    end
    else
      DrawGradientCircle(Canvas, Rect(round(FPosition / 100 * (Width - Height)),
        0, round(FPosition / 100 * (Width - Height)) + (Height), Height),
        (ButtonColor),
        GetShadowColor(ButtonColor));

  end;

  inherited Paint;
end;

procedure TMinimalTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X: integer; Y: integer);
begin
  if Button = mbLeft then
  begin
    FPosition := Math.Max(Min, Math.Min(Max, round(
      ((X - Height / 2) / (Width - Height)) * (Max - Min) + Min)));
    FMouseDown := True;
    Change;
  end;
  FMouseOver := True;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TMinimalTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X: integer; Y: integer);
begin
  if Button = mbLeft then
  begin
    FPosition := Math.Max(Min, Math.Min(Max, round(
      ((X - Height / 2) / (Width - Height)) * (Max - Min) + Min)));
    Change;
    FMouseDown := False;
  end;
  FMouseOver := (x > 0) and (x < Width) and (y > 0) and (y < Height);

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TMinimalTrackBar.MouseMove(Shift: TShiftState; X: integer; Y: integer);
begin
  if ssLeft in Shift then
  begin
    FPosition := Math.Max(Min, Math.Min(Max, round(
      ((X - Height / 2) / (Width - Height)) * (Max - Min) + Min)));
    FMouseDown := True;
    Change;
  end
  else if FMouseDown then
    FMouseDown := False;

  FMouseOver := True;

  inherited MouseMove(Shift, X, Y);
end;

constructor TMinimalTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetInitialBounds(0, 0, 75, 15);
  FMax := 100;
  FMin := 0;
  FPosition := 50;
  FLast := FMin - 1;
  ButtonColor := clSilver;
  Color := $00c39157;
  FGrow := True;
  FButton3D := True;
  ButtonType := tbAuto;
  FMouseOver := False;
  Cursor := crHandPoint;
end;

procedure Register;
begin
  {$I minimaltrackbar_icon.lrs}
  RegisterComponents('Minimal Components', [TMinimalTrackBar]);
end;

end.
