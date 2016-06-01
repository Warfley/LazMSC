unit DataGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Math, contnrs;

type

  TGraphOption = (goLines, goData);
  TGraphOptions = set of TGraphOption;

  TDataSet = class(TObject)
  private
    FList: TList;
    FTag: IntPtr;
    FOnChange: TNotifyEvent;
    FVisible: boolean;
    procedure SetVisible(b: boolean);
    procedure change;
    function GetItem(i: integer): int64;
    procedure setItem(i: integer; val: int64);
    procedure setCount(i: integer);
    function getCount: integer;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function Add(val: int64): integer;
    procedure Delete(i: integer);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    property Visible: boolean read FVisible write setVisible;
    property Position[i: integer]: int64 read GetItem write SetItem; default;
    property Count: integer read GetCount write SetCount;
    property Tag: IntPtr read FTag write FTag;
  end;

  TDataCollection = class(TObject)
  private
    FOnChange: TNotifyEvent;
    FSets: TObjectList;
    FTag: IntPtr;
    procedure Changed(Sender: TObject);
    function getDataSet(i: integer): TDataSet;
    function GetCount: integer;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function Add: TDataSet;
    procedure Delete(i: integer);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    property DataSet[i: integer]: TDataSet read getDataSet; default;
    property Tag: IntPtr read FTag write FTag;
    property Count: integer read GetCount;
  end;


  TChangeEvent = procedure(Sender: TObject; DataSet: TDataSet; Item: integer) of object;

  TDataGraph = class(TGraphicControl)
  private
    FOnChange: TChangeEvent;
    focused: TPoint;
    FData: TDataCollection;
    FMax: int64;
    FMin: int64;
    FOptions: TGraphOptions;
    procedure SetOptions(o: TGraphOptions);
    procedure DataChanged(Sender: TObject);
    procedure SetMax(i: int64);
    procedure SetMin(i: int64);
    function GetDataColor(i: integer): TColor;
    procedure SetDataColor(i: integer; val: TColor);
    { Private declarations }
  protected
    procedure Change(D: TDataSet; i: integer);
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DataSets: TDataCollection read FData;
    property DataColor[i: integer]: TColor read GetDataColor write SetDataColor;
    procedure Clear;
    property Selected: TPoint read focused;
    { Public declarations }
  published
    property Options:TGraphOptions read FOptions write SetOptions;
    property OnChange: TChangeEvent read FOnChange write FOnChange;
    property Max: int64 read FMax write SetMax;
    property Min: int64 read FMin write setMin;
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


procedure TDataSet.SetVisible(b: boolean);
begin
  FVisible := b;
  change;
end;

procedure TDataSet.change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TDataSet.GetItem(i: integer): int64;
begin
  Result := PInt64(FList[i])^;
end;

procedure TDataSet.setItem(i: integer; val: int64);
begin
  PInt64(FList[i])^ := val;
  change;
end;

procedure TDataSet.setCount(i: integer);
begin
  while i > FList.Count do
    Add(0);
  while i < FList.Count do
    Delete(FList.Count - 1);
  change;
end;

function TDataSet.getCount: integer;
begin
  Result := FList.Count;
end;

function TDataSet.Add(val: int64): integer;
var
  tmp: PInt64;
begin
  new(tmp);
  tmp^ := val;
  Result := FList.Add(tmp);
  change;
end;

procedure TDataSet.Delete(i: integer);
begin
  Dispose(PInt64(FList[i]));
  FList.Delete(i);
  change;
end;

procedure TDataSet.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    Dispose(PInt64(FList[i]));
  FList.Clear;
  change;
end;

constructor TDataSet.Create;
begin
  FList := TList.Create;
end;

destructor TDataSet.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TDataCollection.Changed(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Sender);
end;

function TDataCollection.getDataSet(i: integer): TDataSet;
begin
  Result := (FSets[i] as TDataSet);
end;

function TDataCollection.Add: TDataSet;
begin
  Result := TDataSet.Create;
  FSets.Add(Result);
  Result.OnChange := @Changed;
  Result.Visible:=True;
  Changed(Self);
end;

procedure TDataCollection.Delete(i: integer);
begin
  (FSets[i] as TDataSet).Free;
  FSets.Delete(i);
  Changed(Self);
end;

procedure TDataCollection.Clear;
var
  i: integer;
begin
  for i := 0 to FSets.Count - 1 do
    (FSets[i] as TDataSet).Free;
  FSets.Clear;
  Changed(Self);
end;

function TDataCollection.GetCount: integer;
begin
  Result := FSets.Count;
end;

constructor TDataCollection.Create;
begin
  FSets := TObjectList.Create(False);
end;

destructor TDataCollection.Destroy;
begin
  Clear;
  FSets.Free;
  inherited;
end;

procedure TDataGraph.SetOptions(o: TGraphOptions);
begin
  FOptions:=o;
  Invalidate;
end;

procedure TDataGraph.SetMax(i: int64);
begin
  FMax := i;
  Invalidate;
end;

procedure TDataGraph.SetMin(i: int64);
begin
  FMin := i;
  Invalidate;
end;

function TDataGraph.GetDataColor(i: integer): TColor;
begin
  Result := TColor(FData[i].Tag);
end;

procedure TDataGraph.DataChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TDataGraph.SetDataColor(i: integer; val: TColor);
begin
  FData[i].Tag := IntPtr(val);
end;

procedure TDataGraph.Change(D: TDataSet; i: integer);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, D, i);
end;

procedure TDataGraph.Paint;
var
  x, x1, y1, tl, w, i: integer;
  tmp: Int64;
  h: double;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  h := (ClientHeight - 20) / (FMax - FMin);
  Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
  if goLines in FOptions then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Pen.Color:=clSilver;
    Canvas.Brush.Style:=bsClear;
    Canvas.Font:=Self.Font;
    for i:=0 to 10 do
    begin
      Canvas.MoveTo(0, trunc((ClientHeight-20)/10) * i + 10);
      Canvas.LineTo(ClientWidth, trunc((ClientHeight-20)/10) * i + 10);
      if goData in FOptions then
      begin
        tmp:=trunc((10-i)*(FMax-FMin) / 10);
        Canvas.TextOut(0, trunc((ClientHeight-20)/10) * i + 10 - Canvas.TextHeight(IntToStr(tmp)) div 2,
        IntToStr(tmp));
        end;
    end;
  end;
  for i := 0 to FData.Count - 1 do
  begin
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := TColor(FData[i].Tag);

    if not FData[i].Visible then
      Continue;
    w := (ClientWidth - 20) div (FData[i].Count - 1);
    if FData[i].Count > 0 then
      Canvas.MoveTo(10, ClientHeight - trunc(h * (FData[i][0] - FMin) + 10));
    for x := 0 to FData[i].Count - 1 do
    begin
      Canvas.LineTo(w * x + 10, ClientHeight - trunc(h * (FData[i][x] - FMin) + 10));
    end;
  end;
  Canvas.Brush.Color := clBlack;
  Canvas.Brush.Style:=bsSolid;
  for i := 0 to FData.Count - 1 do
  begin
    Canvas.Pen.Color := TColor(FData[i].Tag);
    w := (ClientWidth - 20) div (FData[i].Count - 1);
    if not FData[i].Visible then
      Continue;
    for x := 0 to FData[i].Count - 1 do
    begin
      x1 := w * x + 10;
      y1 := ClientHeight - trunc(h * (FData[i][x] - FMin) + 10);
      if (focused.X = i) and (focused.Y = x) then
        Canvas.Rectangle(x1 - 7, y1 - 7, x1 + 7, y1 + 7)
      else
        Canvas.Rectangle(x1 - 5, y1 - 5, x1 + 5, y1 + 5);
    end;
  end;

  if (focused.X >= 0) and (focused.Y >= 0) and (FData[focused.X].Visible) then
  begin
    w := (ClientWidth - 20) div (FData[focused.X].Count - 1);
    x := focused.Y;
    i := focused.X;
    Canvas.Font.Assign(Font);
    Canvas.Brush.Style := bsClear;
    tl := Canvas.TextWidth(IntToStr(FData[focused.X][focused.Y]));
    if (tl + 8 + w * x + 10) <= ClientWidth then
      Canvas.TextOut(w * x + 18, ClientHeight -
        trunc(h * (FData[i][x] - FMin) + 10) - 7, IntToStr(FData[focused.X][focused.Y]))
    else
      Canvas.TextOut(w * x + 2 - tl, ClientHeight -
        trunc(h * (FData[i][x] - FMin) + 10) - 7, IntToStr(FData[focused.X][focused.Y]));
  end;

  inherited;
end;

procedure TDataGraph.MouseMove(Shift: TShiftState; X, Y: integer);
var
  h: double;
  n, w, i, x1, y1: integer;
begin
  h := (ClientHeight - 20) / (FMax - FMin);
  if (ssLeft in Shift) and (focused.X >= 0) and (focused.Y >= 0) and (FData[focused.X].Visible) then
  begin
    FData[focused.X][focused.Y] :=
      Math.Min(Math.Max(trunc(((ClientHeight-20) - y+10) / h) + FMin, FMin), FMax);
  end
  else
  begin
    focused := Point(-1, -1);
    for i := 0 to FData.Count - 1 do
    begin
      if not FData[i].Visible then
        Continue;
      w := (ClientWidth - 20) div (FData[i].Count - 1);
      for n := 0 to FData[i].Count - 1 do
      begin
        x1 := w * n + 10;
        y1 := ClientHeight - trunc(h * (FData[i][n] - FMin) + 10);
        if (x >= x1 - 7) and (x <= x1 + 7) and (y >= y1 - 7) and (y <= y1 + 7) then
          focused := Point(i, n);
      end;
    end;
  end;
  Invalidate;
  inherited;
end;

destructor TDataGraph.Destroy;
begin
  Clear;
  FData.Free;
  inherited Destroy;
end;

procedure TDataGraph.Clear;
begin
  FData.Clear;
end;

constructor TDataGraph.Create(AOwner: TComponent);
(*var
  i: integer;  *)
begin
  inherited Create(AOwner);
  SetInitialBounds(0, 0, 300, 300);
  FData := TDataCollection.Create;
  FOptions:=[goLines, goData];
  Color := clWhite;
  FMax := 100;
  FMin := 0;
  FData.OnChange := @DataChanged;
  (*with FData.Add do
    for i := 0 to 10 do
      Add(i * 10);
  DataColor[0] := clRed;
  *)
  focused := Point(-1, -1);
end;

procedure Register;
begin
  RegisterComponents('Additional', [TDataGraph]);
end;

end.
