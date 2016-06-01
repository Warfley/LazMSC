unit MinimalPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, DrawUtils;

type

  TBorder = (bdLeft, bdTop, bdBottom, bdRight);
  TBorders = set of TBorder;
  TPictureMode = (pmNone, pmCenter, pmFit, pmStretch, pmRepeatY, pmRepeatX, pmRepeat);

  TMinimalPanel = class(TCustomPanel)
  private
    FBackground: TPicture;
    FBackgroundMode: TPictureMode;
    FBorder: TBorders;
    FBorderWidth: integer;
    FBorderColor: TColor;
    procedure SetBorder(x: TBorders);
    procedure SetBorderWidth(x: integer);
    procedure SetBorderColor(x: TColor);
    procedure SetBackgroundMode(x: TPictureMode);
    procedure ImgChange(Sender: TObject);
    { Private declarations }
  protected
    procedure Paint; override;
    { Protected declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  published
    property Background: TPicture read FBackground write FBackground;
    property BackgroundMode: TPictureMode read FBackgroundMode write SetBackgroundMode;
    property Border: TBorders read FBorder write SetBorder;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    { Published declarations }
  end;

procedure Register;

implementation

constructor TMinimalPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetInitialBounds(0, 0, 200, 140);
  FBackground := TPicture.Create;
  FBackground.OnChange := @ImgChange;
  FBackgroundMode := pmNone;
  Color := clWindow;
  FBorderColor := DarkenColor(clWindow, 40);
  FBorder := [bdBottom, bdTop, bdLeft, bdRight];
  FBorderWidth := 1;
end;

destructor TMinimalPanel.Destroy;
begin
  FBackground.Free;
  inherited Destroy;
end;

procedure TMinimalPanel.SetBackgroundMode(x: TPictureMode);
begin
  FBackgroundMode := x;
  Invalidate;
end;

procedure TMinimalPanel.ImgChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TMinimalPanel.SetBorder(x: TBorders);
begin
  FBorder := x;
  Invalidate;
end;

procedure TMinimalPanel.SetBorderWidth(x: integer);
begin
  FBorderWidth := x;
  Invalidate;
end;

procedure TMinimalPanel.SetBorderColor(x: TColor);
begin
  FBorderColor := x;
  Invalidate;
end;


procedure TMinimalPanel.Paint;
var
  x, y: integer;
begin
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(GetClientRect);
  Canvas.AntialiasingMode := amOn;
  if (FBackground.Height > 0) and (FBackground.Width > 0) then
    case FBackgroundMode of
      pmCenter:
      begin
        Canvas.Draw(Self.Width div 2 - FBackground.Width div 2,
          Self.Height div 2 - FBackground.Height div 2, FBackground.Graphic);
      end;
      pmFit:
      begin
        if FBackground.Width < FBackground.Height then
        begin
          x := Self.Width;
          y := round(Self.Width / Background.Width * FBackground.Height);
          Canvas.StretchDraw(Rect(Width div 2 - x div 2, Height div
            2 - y div 2, Width div 2 + x div 2, Height div 2 + y div 2),
            FBackground.Graphic);
        end
        else
        begin
          y := Self.Height;
          x := round(Self.Height / Background.Height * FBackground.Width);
          Canvas.StretchDraw(Rect(Width div 2 - x div 2, Height div
            2 - y div 2, Width div 2 + x div 2, Height div 2 + y div 2),
            FBackground.Graphic);
        end;
      end;
      pmStretch:
      begin
        Canvas.StretchDraw(GetClientRect, FBackground.Graphic);
      end;
      pmNone:
      begin
        Canvas.Draw(0, 0, FBackground.Graphic);
      end;
      pmRepeat:
      begin
        y := 0;
        while y < Self.Height do
        begin
          x := 0;
          while x < Self.Width do
          begin
            Canvas.Draw(x, y, FBackground.Graphic);
            Inc(x, FBackground.Width);
          end;
          Inc(y, FBackground.Height);
        end;
      end;
      pmRepeatX:
      begin
        x := 0;
        while x < Self.Width do
        begin
          Canvas.Draw(x, 0, FBackground.Graphic);
          Inc(x, Background.Width);
        end;
      end;
      pmRepeatY:
      begin
        y := 0;
        while y < Self.Height do
        begin
          Canvas.Draw(0, y, FBackground.Graphic);
          Inc(y, Background.Height);
        end;
      end;
    end;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FBorderColor;
  Canvas.Pen.Style := psClear;

  if bdLeft in FBorder then
    Canvas.FillRect(0, 0, FBorderWidth, Height);

  if bdRight in FBorder then
    Canvas.FillRect(Width - FBorderWidth, 0, Width, Height);

  if bdTop in FBorder then
    Canvas.FillRect(0, 0, Width, FBorderWidth);

  if bdBottom in FBorder then
    Canvas.FillRect(0, Height - FBorderWidth, Width, Height);

  Canvas.Font := Self.Font;
  Canvas.Brush.Style := bsClear;

  DrawTextCenter(Canvas, Width, Height, Caption);


  if Assigned(OnPaint) then
    OnPaint(Self);
end;


procedure Register;
begin
  {$I minimalpanel_icon.lrs}
  RegisterComponents('Minimal Components', [TMinimalPanel]);
end;

end.
