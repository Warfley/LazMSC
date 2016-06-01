unit MinimalCheckbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DrawUtils,
  Math;

type
  TCheckType = (ctCheck, ctFill, ctCross);

  TMinimalCheckbox = class(TGraphicControl)
  private
    FChecked: boolean;
    FActiveColor: TColor;
    FCheckType: TCheckType;
    FMouseOver: boolean;
    procedure SetChecked(b: boolean);
    procedure SetActiveColor(c: TColor);
    procedure setCheckType(c: TCheckType);
    { Private declarations }
  protected
    procedure Click; override;
    procedure Paint; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  published
    property ActiveColor: TColor read FActiveColor write SetActiveColor;
    property Checked: boolean read FChecked write SetChecked;
    property CheckType: TCheckType read FCheckType write setCheckType;
    property MouseOver: boolean read FMouseOver;
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

procedure Register;
begin
  {$I minimalcheckbox_icon.lrs}
  RegisterComponents('Minimal Components', [TMinimalCheckbox]);
end;


procedure TMinimalCheckbox.SetChecked(b: boolean);
begin
  FChecked := b;
  Invalidate;
end;

procedure TMinimalCheckbox.SetActiveColor(c: TColor);
begin
  FActiveColor := c;
  Invalidate;
end;

procedure TMinimalCheckbox.setCheckType(c: TCheckType);
begin
  FCheckType := c;
  Invalidate;
end;

procedure TMinimalCheckbox.Click;
begin
  FChecked := not FChecked;
  Invalidate;
  inherited;
end;

constructor TMinimalCheckbox.Create(AOwner: TComponent);
begin
  inherited;
  SetInitialBounds(0, 0, 75, 24);
  Color := TColor($00F0F0F0);
  FActiveColor := TColor($00FF9933);
  FChecked := False;
  FCheckType := ctCheck;
  FMouseOver := False;
end;

procedure TMinimalCheckbox.Paint;

  procedure DrawCheck(Col: TColor);
  var
    q: integer;
  begin
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := Max(Height div 12, 1);
    Canvas.Pen.Color := Col;
    q := Self.Height div 4;
    Canvas.MoveTo(q + 1, q * 2 + 1);
    Canvas.LineTo(q * 2 + 1, q * 3 + 1);
    Canvas.LineTo(q * 3 + 1, q + 1);
  end;

  procedure DrawCross(Col: TColor);
  var
    q: integer;
  begin
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := Max(Height div 12, 1);
    Canvas.Pen.Color := Col;
    q := Self.Height div 4;
    Canvas.MoveTo(q + 1, q + 1);
    Canvas.LineTo(q * 3 + 1, q * 3 + 1);
    Canvas.MoveTo(q * 3 + 1, q + 1);
    Canvas.LineTo(q + 1, q * 3 + 1);
  end;

  procedure DrawFill(Col: TColor);
  begin
    Canvas.Pen.Style := psClear;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Col;
    Canvas.Rectangle(0, 0, Self.Height, Self.Height);
  end;

var
  DrawCol: TColor;
begin
  DrawCol := ifthen(Enabled, FActiveColor, DarkenColor(Color, 20));
  // Drawing Check Rect
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Pen.Style := psSolid;
    Pen.Color := ifthen(MouseOver, FActiveColor, DarkenColor(Color, 20));
    Pen.Width := ifthen(MouseOver, 2, 1);
    Rectangle(1, 1, Self.Height - 1, Self.Height - 1);
    // Drawing Checkmark
    if FChecked then
      case FCheckType of
        ctCheck: DrawCheck(DrawCol);
        ctCross: DrawCross(DrawCol);
        ctFill: DrawFill(DrawCol);
      end;
    // Drawing Text
    Font := Self.Font;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(Self.Height + 4, (Self.Height div 2) -
      (TextHeight(Caption) div 2 + 1), Caption);
  end;

  inherited;
end;

procedure TMinimalCheckbox.MouseEnter;
begin
  FMouseOver := True;
  Invalidate;
  inherited;
end;

procedure TMinimalCheckbox.MouseLeave;
begin
  FMouseOver := False;
  Invalidate;
  inherited;
end;


end.
