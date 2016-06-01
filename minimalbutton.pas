unit MinimalButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, DrawUtils,
  GraphUtil, Dialogs, Math;

type
  TMButtonStyle = (bsNormal, bsGradient, bsNone, bsFlat, bsFlatBorder,
    bsBorderless, bsBorderOnly);

  TMinimalButton = class(TGraphicControl)
  private
    FEdgeRound: byte;
    FBorder: TColor;
    FBorderSize: byte;
    FButtonType: TMButtonStyle;
    FMouseOver: boolean;
    FMousePressed: boolean;
    procedure SetStyle(a: TMButtonStyle);
    procedure SetRadius(b: byte);
    procedure setBorder(c: TColor);
    procedure setBorderSize(s: byte);
    { Private declarations }
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: integer; Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X: integer; Y: integer); override;
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure Click; override;

    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  published
    property ButtonType: TMButtonStyle read FButtonType write SetStyle;
    property Border: TColor read FBorder write setBorder;
    property MouseOver: boolean read FMouseOver;
    property MousePressed: boolean read FMousePressed;
    property Radius: byte read FEdgeRound write SetRadius;
    property BorderWidth: byte read FBorderSize write setBorderSize;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
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

procedure TMinimalButton.Click;
begin
  Invalidate;
  inherited Click;
end;

procedure TMinimalButton.SetStyle(a: TMButtonStyle);
begin
  FButtonType := a;
  if a = bsGradient then
    FBorder := GetShadowColor(Color)
  else
    FBorder := $00484848;
  Invalidate;
end;

procedure TMinimalButton.SetRadius(b: byte);
begin
  FEdgeRound := b;
  Invalidate;
end;

procedure TMinimalButton.setBorder(c: TColor);
begin
  FBorder := c;
  Invalidate;
end;

procedure TMinimalButton.setBorderSize(s: byte);
begin
  FBorderSize := s;
  Invalidate;
end;

constructor TMinimalButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetInitialBounds(0, 0, 75, 25);
  Color := clWhite;
  Border := clBlack;
end;

procedure TMinimalButton.Paint;
begin
    Canvas.Brush.Color := ColorToRGB(Color);
    Canvas.Pen.Color := FBorder;
    Canvas.Pen.Width := FBorderSize;
    case FButtonType of
      bsBorderless:
      begin
        if FMousePressed then
          Canvas.Brush.Color := DarkenColor(Color, 40)
        else if FMouseOver then
          Canvas.Brush.Color := LightenColor(Color, 40);
        Canvas.Pen.Style := psClear;
        Canvas.Brush.Style := bsSolid;
      end;
      bsNone:
      begin
        Canvas.Pen.Style := psClear;
        Canvas.Brush.Style := bsClear;
      end;
      bsBorderOnly:
      begin
        if FMousePressed then
          Canvas.Pen.Color := DarkenColor(FBorder, 10)
        else if FMouseOver then
          Canvas.Pen.Color := LightenColor(FBorder, 15);
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Style := psSolid;
      end;
      bsFlat:
      begin
        Canvas.Pen.Style := psClear;
        if FMousePressed then
        begin
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := DarkenColor(Color, 10);
        end
        else if FMouseOver then
          Canvas.Brush.Style := bsSolid
        else
          Canvas.Brush.Style := bsClear;
      end;
      bsFlatBorder:
      begin
        Canvas.Brush.Style := bsClear;
        if FMousePressed then
        begin
          Canvas.Pen.Style := psSolid;
          Canvas.Pen.Color := DarkenColor(Border, 10);
        end
        else if FMouseOver then
          Canvas.Pen.Style := psSolid
        else
          Canvas.Pen.Style := psClear;
      end;
      bsNormal:
      begin
        if FMousePressed then
        begin
          Canvas.Pen.Color := (FBorder);
          Canvas.Brush.Color := DarkenColor(Color, 10);
        end
        else if FMouseOver then
        begin
          Canvas.Pen.Color := (FBorder);
          Canvas.Brush.Color := LightenColor(Color, 15);
        end;
        Canvas.Brush.Style := bsSolid;
        Canvas.Pen.Style := psSolid;
      end;
    end;

    if not Enabled then
    begin
      Canvas.Brush.Color := BlackNWhite(Color);
      Canvas.Pen.Color := BlackNWhite(Border);
    end;
    if FButtonType = bsGradient then
    begin
      if FMousePressed then
      begin
        Canvas.Brush.Color := DarkenColor(Color, 10);
        Canvas.Pen.Color := DarkenColor(Border, 10);
      end
      else if FMouseOver then
      begin
        Canvas.Brush.Color := LightenColor(Color, 15);
        Canvas.Pen.Color := LightenColor(Border, 15);
      end;
      DrawVerticalGradient(Canvas, Rect(0, 0, Width, Height - FBorderSize + 1),
        Canvas.Brush.Color, Canvas.Pen.Color);
    end
    else
      Canvas.RoundRect(0, 0, Width, Height, Radius, Radius);
    Canvas.Font := Font;
    Canvas.Brush.Style := bsClear;
    DrawTextCenter(Canvas, Width, Height, Caption);

  inherited Paint;
end;

procedure TMinimalButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X: integer; Y: integer);
begin
  if Button = mbLeft then
    FMousePressed := True;
  Invalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TMinimalButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X: integer; Y: integer);
begin
  if Button = mbLeft then
    FMousePressed := False;
  Invalidate;
  inherited mouseUp(Button, Shift, X, Y);
end;

procedure TMinimalButton.MouseLeave;
begin
  FMouseOver := False;
  inherited MouseLeave;
  Invalidate;
end;

procedure TMinimalButton.MouseEnter;
begin
  FMouseOver := True;
  Invalidate;
  inherited MouseEnter;
end;

procedure Register;
begin
  {$I minimalbutton_icon.lrs}
  RegisterComponents('Minimal Components', [TMinimalButton]);
end;

end.
