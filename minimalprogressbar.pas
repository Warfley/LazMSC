unit MinimalProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DrawUtils, Math;

type

  TCaptionType = (ctNone, ctValue, ctPercent);

  TMinimalProgressBar = class(TGraphicControl)
  private
    FCaption: TCaptionType;
    FMouseOver: boolean;
    FBackground: TColor;
    FMin, FMax, FPosition: integer;
    FCaption_: string;
    FRadius: integer;
    procedure SetRadius(x: integer);
    procedure SetPosition(x: integer);
    procedure SetMin(x: integer);
    procedure SetMax(x: integer);
    { Private declarations }
  protected
    procedure Paint; override;
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure Resize; override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  published
    property Background: TColor read FBackground write FBackground;
    property Radius: integer read FRadius write SetRadius;
    property MouseOver: boolean read FMouseOver;
    property Position: integer read FPosition write SetPosition;
    property Max: integer read FMax write SetMax;
    property Min: integer read FMin write SetMin;
    property Caption_: string read FCaption_ write FCaption_;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption: TCaptionType read FCaption write FCaption;
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

constructor TMinimalProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetInitialBounds(0, 0, 120, 14);
  FMax := 100;
  Color := clRed;
  Font.Size := 12;
  FCaption_ := '';
  FPosition := 50;
  FRadius := -1;
  Caption := ctPercent;
  FBackground := clSilver;
end;

procedure TMinimalProgressBar.SetRadius(x: integer);
begin
  FRadius := x;
end;

procedure TMinimalProgressBar.SetPosition(x: integer);
begin
  if (x >= FMin) and (x <= FMax) then
    FPosition := x;
  Invalidate;
end;

procedure TMinimalProgressBar.SetMin(x: integer);
begin
  if FMax - FMin > 0 then
    FMin := x;
  Invalidate;
end;

procedure TMinimalProgressBar.SetMax(x: integer);
begin
  if FMax - FMin > 0 then
    FMax := x;
  Invalidate;
end;

procedure TMinimalProgressBar.Resize;
begin
  Self.Height := Math.Max(Math.Max(Height, 10), Self.Font.Height);
  Self.Height := Math.Min(Height, Width div 3);
  Invalidate;
  inherited Resize;
end;

procedure TMinimalProgressBar.Paint;
var
  cap: string;
  r: integer;
  o: integer;
begin
  with Canvas do
  begin
    if Radius = -1 then
    begin
      r := 5;
      if Self.Height <= 18 then
        r := Self.Height - 4;
    end
    else
      r := Radius;
    o := ifthen(FCaption = ctNone, 0, 4);
    Brush.Style := bsSolid;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Color := FBackground;
    Pen.Color := DarkenColor(Brush.Color, 20);
    RoundRect(0, o, Self.Width, Self.Height - (o + 1), r, r);
    if Enabled then
    begin
      Brush.Color := Self.Color;
      Pen.Style := psClear;
      RoundRect(0, o, Round((FPosition - FMin) / (FMax - FMin) * (Self.Width)),
        Self.Height - o, r, r);
      Brush.Style := bsClear;
      Font.Assign(Self.Font);
    end;
  end;
  if MouseOver and Enabled then
  begin
    case Caption of
      ctNone: cap := FCaption_;
      ctPercent: cap := IntToStr(Round((FPosition - FMin) / (FMax - FMin) *
          (100))) + '%' + FCaption_;
      ctValue: cap := IntToStr(FPosition) + FCaption_;
    end;
    DrawTextCenter(Canvas, Width, Height, cap);
  end;

  inherited Paint;
end;

procedure TMinimalProgressBar.MouseLeave;
begin
  FMouseOver := False;
  Invalidate;
  inherited MouseLeave;
end;

procedure TMinimalProgressBar.MouseEnter;
begin
  FMouseOver := True;
  Invalidate;
  inherited MouseEnter;
end;

procedure Register;
begin
  {$I minimalprogressbar_icon.lrs}
  RegisterComponents('Minimal Components', [TMinimalProgressBar]);
end;

end.
