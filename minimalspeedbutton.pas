unit MinimalSpeedButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  DrawUtils;

type
  TTextPosition = (tpLeft, tpRight, tpTop, tpBottom);

  TMinimalSpeedButton = class(TGraphicControl)
  private
    FBorderWidth: integer;
    FPicture: TPicture;
    FMouseDown: boolean;
    FMouseOver: boolean;
    FFlat: boolean;
    FBorder: TColor;
    FTextPosition: TTextPosition;
    FGlyphCount: integer;
    FGlyph: integer;
    FGlyphHeight, FGlyphWidth: integer;
    procedure SetBorderWidth(i: integer);
    procedure SetGlyphHeight(i: integer);
    procedure SetGlyphWidth(i: integer);
    procedure setBorder(c: TColor);
    procedure setGlyph(g: integer);
    procedure setGlyphCount(g: integer);
    procedure setFlat(c: boolean);
    procedure setPicture(c: TPicture);
    procedure setTextPosition(c: TTextPosition);
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
    destructor Destroy; override;
    { Public declarations }
  published
    property BorderWidth: integer read FBorderWidth write FBorderWidth;
    property GlyphWidth: integer read FGlyphWidth write SetGlyphWidth;
    property GlyphHeight: integer read FGlyphHeight write SetGlyphHeight;
    property Picture: TPicture read FPicture write setPicture;
    property Flat: boolean read FFlat write setFlat;
    property Border: TColor read FBorder write setBorder;
    property MousePressed: boolean read FMouseDown;
    property MouseOver: boolean read FMouseOver;
    property TextPosition: TTextPosition read FTextPosition write setTextPosition;
    property Glyph: integer read FGlyph write SetGlyph;
    property GlyphCount: integer read FGlyphCount write SetGlyphCount;
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
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I minimalspeedbutton_icon.lrs}
  RegisterComponents('Minimal Components', [TMinimalSpeedButton]);
end;

procedure TMinimalSpeedButton.setGlyph(g: integer);
begin
  if g >= 0 then
    FGlyph := g;
  Invalidate;
end;

procedure TMinimalSpeedButton.setGlyphCount(g: integer);
begin
  if g > 0 then
    FGlyphCount := g;
  Invalidate;
end;

procedure TMinimalSpeedButton.setBorder(c: TColor);
begin
  FBorder := c;
  Invalidate;
end;

procedure TMinimalSpeedButton.setFlat(c: boolean);
begin
  FFlat := c;
  Invalidate;
end;

procedure TMinimalSpeedButton.setPicture(c: TPicture);
begin
  FPicture.Assign(c);
  Invalidate;
end;

procedure TMinimalSpeedButton.setTextPosition(c: TTextPosition);
begin
  FTextPosition := c;
  Invalidate;
end;

procedure TMinimalSpeedButton.SetGlyphHeight(i: integer);
begin
  if i >= 0 then
    FGlyphHeight := i;
  Invalidate;
end;

procedure TMinimalSpeedButton.SetGlyphWidth(i: integer);
begin
  if i > 0 then
    FGlyphWidth := i;
  Invalidate;
end;

procedure TMinimalSpeedButton.Paint;
var
  bmp: TBitmap;
begin
  Canvas.Pen.Color := Border;
  Canvas.pen.Style := psClear;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  if MousePressed then
  begin
    Canvas.Brush.Color := DarkenColor(Color, 10);
  end
  else if MouseOver then
  begin
    Canvas.Brush.Color := LightenColor(Color, 15);
  end;
  Canvas.Rectangle(0, 0, Width, Height);
  Font := Self.Font;
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf32bit;
    bmp.Width := FPicture.Width div FGlyphCount;
    bmp.Height := FPicture.Height;
    bmp.Canvas.Draw(-Glyph * bmp.Width, 0, Picture.Graphic);
    if Assigned(Picture.Graphic) then
      Canvas.StretchDraw(Rect(Width div 2 - FGlyphWidth div 2,
        Height div 2 - FGlyphHeight div 2, Width div 2 + FGlyphWidth div 2,
        Height div 2 + FGlyphHeight div 2), bmp);
  finally
    bmp.Free;
  end;

  case TextPosition of
    tpBottom: Canvas.TextOut(Width div 2 - Canvas.TextWidth(Caption) div 2,
        Height div 2 + Picture.Height div 2 + 2, Caption);
    tpLeft: Canvas.TextOut(Width div 2 - Picture.Width div 2 -
        Canvas.TextWidth(Caption) - 2,
        Height div 2 + Canvas.TextHeight(Caption) div 2, Caption);
    tpRight: Canvas.TextOut(Width div 2 + Picture.Width div 2 + 2,
        Height div 2 + Canvas.TextHeight(Caption) div 2, Caption);
    tpTop: Canvas.TextOut(Width div 2 - Canvas.TextWidth(Caption) div
        2, Height div 2 - Picture.Height div 2 - Canvas.TextHeight(Caption) -
        2, Caption);
  end;

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Width := BorderWidth;
  if Flat and (not MousePressed) then
    Canvas.Pen.Style := psClear
  else
  if BorderWidth > 0 then
    Canvas.Pen.Style := psSolid;
  Canvas.Rectangle(BorderWidth div 2, BorderWidth div 2, Width, Height);
  inherited;
end;

procedure TMinimalSpeedButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X: integer; Y: integer);
begin
  inherited;
  if Button = mbLeft then
    FMouseDown := True;
  Invalidate;
end;

procedure TMinimalSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X: integer; Y: integer);
begin
  inherited;
  if Button = mbLeft then
    FMouseDown := False;
  Invalidate;
end;

procedure TMinimalSpeedButton.MouseLeave;
begin
  inherited;
  FMouseOver := False;
  Invalidate;
end;

procedure TMinimalSpeedButton.MouseEnter;
begin
  inherited;
  FMouseOver := True;
  Invalidate;
end;

procedure TMinimalSpeedButton.Click;
begin
  inherited;
  Invalidate;
end;

procedure TMinimalSpeedButton.SetBorderWidth(i: integer);
begin
  if i > 1 then
    FBorderWidth := i;
  Invalidate;
end;

constructor TMinimalSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
  SetInitialBounds(0, 0, 24, 24);
  FGlyph := 0;
  FGlyphCount := 1;
  Caption := '';
  FTextPosition := tpRight;
  FFlat := False;
  FBorder := clBlack;
  FMouseOver := False;
  FMouseDown := False;
  GlyphWidth := 24;
  GlyphHeight := 24;
  Color := clWhite;
  FBorderWidth := 1;
end;

destructor TMinimalSpeedButton.Destroy;
begin
  FPicture.Free;
  inherited;
end;


end.
