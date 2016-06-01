unit MinimalCheckboxGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics,
  Dialogs, contnrs, MinimalCheckbox, DrawUtils, Math;

type

  TIndexEvent = procedure(Sender: TObject; Index: integer) of object;

  TCheckType = MinimalCheckbox.TCheckType;

  TBorderStyle = bsNone..bsSingle;

  TMinimalCheckboxGroup = class(TScrollingWinControl)
  private
    FBoxes: TObjectList;
    FOnChange: TIndexEvent;
    FItems: TStringList;
    FActiveColor: TColor;
    FCheckType: TCheckType;
    FBorderStyle: TBorderStyle;
    FSingleSelection: boolean;
    FItemHeight: integer;
    procedure SetItemTag(i: integer; x: IntPtr);
    function GetItemTag(i: integer): IntPtr;
    procedure SetItemHeight(x: integer);
    procedure SetSingleSelection(b: boolean);
    function GetChecked(x: integer): boolean;
    procedure SetChecked(x: integer; b: boolean);
    procedure UpdateList(Sender: TObject);
    procedure HandleCheck(Sender: TObject);
    procedure SetActiveColor(c: TColor);
    procedure setCheckType(c: TCheckType);
    procedure SetItems(i: TStringList);
    procedure SetBorderStyle(b: TBorderStyle);
    function GetItemEnabled(i: integer): boolean;
    procedure SetItemEnabled(i: integer; b: boolean);
    { Private declarations }
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure Click; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    { Protected declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Checked[i: integer]: boolean read GetChecked write SetChecked; default;
    property ItemTag[i: integer]: IntPtr read GetItemTag write SetItemTag;
    property ItemEnabled[i: integer]: boolean read GetItemEnabled write SetItemEnabled;
    { Public declarations }
  published
    property OnChange: TIndexEvent read FOnChange write FOnChange;
    property ItemHeight: integer read FItemHeight write SetItemHeight;
    property ActiveColor: TColor read FActiveColor write SetActiveColor;
    property CheckType: TCheckType read FCheckType write setCheckType;
    property SingleSelection: boolean read FSingleSelection write SetSingleSelection;
    property Items: TStringList read FItems write SetItems;
    property Align;
    property AutoScroll;
    property VertScrollBar;
    property HorzScrollBar;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
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
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I minimalcheckboxgroup_icon.lrs}
  RegisterComponents('Minimal Components', [TMinimalCheckboxGroup]);
end;


procedure TMinimalCheckboxGroup.SetItemTag(i: integer; x: IntPtr);
begin
  (FBoxes[i] as TMinimalCheckbox).Tag := x;
end;

function TMinimalCheckboxGroup.GetItemTag(i: integer): IntPtr;
begin
  Result := (FBoxes[i] as TMinimalCheckbox).Tag;
end;

procedure TMinimalCheckboxGroup.SetBorderStyle(b: TBorderStyle);
begin
  FBorderStyle := b;
  Invalidate;
end;

procedure TMinimalCheckboxGroup.SetItemHeight(x: integer);
var
  i: integer;
begin
  for i := 0 to FBoxes.Count - 1 do
  begin
    (FBoxes[i] as TMinimalCheckbox).Height := x;
    (FBoxes[i] as TMinimalCheckbox).Top := x * i;
  end;
  FItemHeight := x;
  Invalidate;
end;

procedure TMinimalCheckboxGroup.SetSingleSelection(b: boolean);
var
  i: integer;
begin
  if b then
    for i := 0 to FBoxes.Count - 1 do
      (FBoxes[i] as TMinimalCheckbox).Checked := False;
  FSingleSelection := b;
  Invalidate;
end;

function TMinimalCheckboxGroup.GetChecked(x: integer): boolean;
begin
  Result := (FBoxes[x] as TMinimalCheckbox).Checked;
end;

procedure TMinimalCheckboxGroup.SetChecked(x: integer; b: boolean);
var
  i: integer;
begin
  if SingleSelection and b then
  begin
    for i := 0 to FBoxes.Count - 1 do
      if i <> x then
        (FBoxes[i] as TMinimalCheckbox).Checked := False;
    (FBoxes[x] as TMinimalCheckbox).Checked := b;
  end
  else
    (FBoxes[x] as TMinimalCheckbox).Checked := b;
  Invalidate;
end;

procedure TMinimalCheckboxGroup.UpdateList(Sender: TObject);
var
  tmp: TMinimalCheckbox;
  i: integer;
begin
  while FBoxes.Count > FItems.Count do
  begin
    FBoxes[FBoxes.Count - 1].Free;
    FBoxes.Delete(FBoxes.Count - 1);
  end;
  while FBoxes.Count < FItems.Count do
  begin
    tmp := TMinimalCheckbox.Create(Self);
    tmp.Parent := Self;
    tmp.Name := Name + 'Item' + IntToStr(FBoxes.Count);
    tmp.Height := FItemHeight;
    tmp.Width := Self.ClientWidth - 1;
    tmp.Caption := FItems[FBoxes.Count];
    tmp.Checked := False;
    tmp.ActiveColor := FActiveColor;
    tmp.CheckType := FCheckType;
    tmp.Left := 1;
    tmp.Color := Color;
    tmp.Top := FBoxes.Count * FItemHeight + 1;
    tmp.Visible := True;
    tmp.OnClick := @HandleCheck;
    FBoxes.Add(tmp);
  end;
  for i := 0 to FBoxes.Count - 1 do
  begin
    (FBoxes[i] as TMinimalCheckbox).Caption := FItems[i];
    (FBoxes[i] as TMinimalCheckbox).Width := Self.ClientWidth - 1;
  end;
end;

procedure TMinimalCheckboxGroup.HandleCheck(Sender: TObject);
var
  i: integer;
begin
  if SingleSelection then
    for i := 0 to FBoxes.Count - 1 do
      if FBoxes[i] <> Sender then
        (FBoxes[i] as TMinimalCheckbox).Checked := False;
  Self.SetFocus;
  if Assigned(FOnChange) then
    FOnChange(Self, (Sender as TMinimalCheckbox).Top div FItemHeight);
  Invalidate;
end;

procedure TMinimalCheckboxGroup.SetActiveColor(c: TColor);
var
  i: integer;
begin
  for i := 0 to FBoxes.Count - 1 do
    (FBoxes[i] as TMinimalCheckbox).ActiveColor := c;
  FActiveColor := c;
  Invalidate;
end;

procedure TMinimalCheckboxGroup.setCheckType(c: TCheckType);
var
  i: integer;
begin
  for i := 0 to FBoxes.Count - 1 do
    (FBoxes[i] as TMinimalCheckbox).CheckType := c;
  FCheckType := c;
  Invalidate;
end;

procedure TMinimalCheckboxGroup.SetItems(i: TStringList);
begin
  FItems.Assign(i);
  FItems.OnChange := @UpdateList;
end;


function TMinimalCheckboxGroup.GetItemEnabled(i: integer): boolean;
begin
  Result := (FBoxes[i] as TMinimalCheckbox).Enabled;
end;

procedure TMinimalCheckboxGroup.SetItemEnabled(i: integer; b: boolean);
begin
  (FBoxes[i] as TMinimalCheckbox).Enabled := b;
end;

procedure TMinimalCheckboxGroup.DoEnter;
begin
  Invalidate;
  inherited;
end;

procedure TMinimalCheckboxGroup.DoExit;
begin
  Invalidate;
  inherited;
end;

procedure TMinimalCheckboxGroup.Click;
begin
  SetFocus;
  Invalidate;
end;

procedure TMinimalCheckboxGroup.Paint;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    if BorderStyle = bsSingle then
      Pen.Style := psSolid
    else
      Pen.Style := psClear;
    Brush.Color := Color;
    Pen.Color := ifthen(Focused, ActiveColor, DarkenColor(Color, 20));
    Pen.Width := 1;
    Rectangle(0, VertScrollBar.Position, Self.ClientWidth,
      Max(Self.ClientHeight, FBoxes.Count * FItemHeight));
  end;
  inherited;
end;

procedure TMinimalCheckboxGroup.Resize;
var
  i: integer;
begin
  if Assigned(FBoxes) then
    for i := 0 to FBoxes.Count - 1 do
      (FBoxes[i] as TMinimalCheckbox).Width := Self.ClientWidth - 1;
  Invalidate;
end;

constructor TMinimalCheckboxGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBoxes := TObjectList.Create(False);
  FItems := TStringList.Create;
  SetInitialBounds(0, 0, 200, 140);
  Color := TColor($00F0F0F0);
  FSingleSelection := False;
  FItemHeight := 24;
  FBorderStyle := bsSingle;
  FActiveColor := TColor($00FF9933);
  FCheckType := ctFill;
  FItems.OnChange := @UpdateList;
  VertScrollBar.Smooth := True;
  VertScrollBar.Tracking := True;
  HorzScrollBar.Visible := False;
end;

destructor TMinimalCheckboxGroup.Destroy;
var
  i: integer;
begin
  for i := 0 to FBoxes.Count - 1 do
    FBoxes[i].Free;
  FBoxes.Free;
  FItems.Free;
  inherited;
end;

end.
