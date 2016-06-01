unit DrawUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math, LCLType, GraphUtil;

type
  TPercent = 0..100;
  TRGB = packed record
    case byte of
      0: (R, G, B: byte);
      1: (Color: TColor);
  end;

  THLS = packed record
    H,S: Byte;
    case boolean of
      True: (V: byte);
      False: (L: byte);
  end;
  THSV=THLS;

function LightenColor(c: TColor; Val: TPercent): TColor;
function DarkenColor(c: TColor; Val: TPercent): TColor;
function BlackNWhite(c: TColor): TColor;
procedure DrawTextCenter(c: TCanvas; Width, Height: integer; Caption: string);
function RGBtoHSV(Col: TColor): THSV;
function HSVtoRGB(HSV: THSV): TColor;
procedure DrawGradientCircle(Canvas: TCanvas; ARect: TRect;
  Color1, Color2: TColor; Vert: boolean = True); overload;
procedure DrawGradientCircle(Canvas: TCanvas; Left, Top, Radius: Integer;
  Color1, Color2: TColor; Vert: boolean = True); overload;
procedure DrawRadient(Canvas: TCanvas; Left, Top, Radius: Integer; Color1, Color2: TColor);
procedure FillRadient(Canvas: TCanvas; ARect: TRect; Color1, Color2: TColor);
procedure DrawCircle(Canvas: TCanvas; Left, Top, Radius: Integer); inline;
procedure DrawOpacity(PosX, PosY: Integer; c: TCanvas; g: TGraphic; Opac: double);

implementation

procedure DrawOpacity(PosX, PosY: integer; c: TCanvas; g: TGraphic; Opac: double);
var
  x, y: integer;
  bmp: TBitmap;
  r, r2: TRGB;
begin
  bmp := TBitmap.Create;
  try
    bmp.Assign(g);
    for y := 0 to bmp.Height do
      for x := 0 to bmp.Width do
      begin
        r.Color := c.Pixels[PosX + X, PosY + y];
        r2.Color := bmp.Canvas.Pixels[X, Y];
        r.R:=round(r.R*(1-Opac)+r2.R*Opac);
        r.G:=round(r.G*(1-Opac)+r2.G*Opac);
        r.B:=round(r.B*(1-Opac)+r2.B*Opac);
        c.Pixels[PosX + X, PosY + y]:=r.Color;
      end;
  finally
    bmp.Free;
  end;
end;

procedure DrawCircle(Canvas: TCanvas; Left, Top, Radius: Integer); inline;
begin
  Canvas.Ellipse(Left-Radius, Top-Radius, Left+Radius, Top+Radius);
end;

procedure DrawRadient(Canvas: TCanvas; Left, Top, Radius: Integer; Color1, Color2: TColor);
  function GetColVal(C1, C2, F1, F2: Integer): TColor;
  var r1, r2, res: TRGB;
  begin
    r1.Color:=C1;
    r2.Color:=C2;
    res.r := Round(r1.r + (r2.r - r1.r) * F1 / (F2));
    res.g := Round(r1.g + (r2.g - r1.g) * F1 / (F2));
    res.b := Round(r1.b + (r2.b - r1.b) * F1 / (F2));
    Result:=res.Color;
  end;

var i: Integer;
begin
  for i := Radius downto 0 do
  begin
    Canvas.Brush.Color:=GetColVal(Color2, Color1, i, Radius);
    Canvas.Brush.Style:=bsSolid;
    Canvas.Pen.Style:=psClear;
    DrawCircle(Canvas, Left, Top, i);
  end;
end;

procedure FillRadient(Canvas: TCanvas; ARect: TRect; Color1, Color2: TColor);
var bmp: TBitmap;
begin
  bmp:=TBitmap.Create;
  try
    bmp.Width:=ARect.Right-ARect.Left;
    bmp.Height:=ARect.Bottom-ARect.Top;
    DrawRadient(bmp.Canvas, bmp.Width div 2, bmp.Height div 2,
      round(sqrt(bmp.Height**2 + bmp.Width**2) / 2), Color1, Color2);
    Canvas.Draw(ARect.Left, ARect.Top, bmp);
  finally
    bmp.Free;
  end;
end;

procedure DrawGradientCircle(Canvas: TCanvas; Left, Top, Radius: Integer;
  Color1, Color2: TColor; Vert: boolean = True);
begin
  DrawGradientCircle(Canvas, Rect(Left-Radius, Top-Radius,
    Left+Radius, Top+Radius), Color1, Color2, Vert);
end;

procedure DrawGradientCircle(Canvas: TCanvas; ARect: TRect;
  Color1, Color2: TColor; Vert: boolean = True);
  function GetRectV(Circle: TRect; Position: integer): TRect;
  var
    Mid, Wid, r: integer;
  begin
    Mid := (Circle.Right + Circle.Left) div 2;
    r := abs(Circle.Right - Circle.Left) div 2;
    Wid := trunc(sqrt(sqr(r) - sqr(Position - r)));
    if Position - r = 0 then
      wid -= 1;
    if Wid = 1 then
      Wid := 0;
    Result.Top := Position + Circle.Top;
    Result.Bottom := Result.Top + 1;
    Result.Left := Mid - Wid;
    Result.Right := Mid + Wid;
  end;

  function GetRectH(Circle: TRect; Position: integer): TRect;
  var
    Mid, Wid, r: integer;
  begin
    Mid := (Circle.Bottom + Circle.Top) div 2;
    r := abs(Circle.Bottom - Circle.Top) div 2;
    Wid := trunc(sqrt(sqr(r) - sqr(Position - r)));
    if Position - r = 0 then
      wid -= 1;
    if Wid = 1 then
      Wid := 0;
    Result.Left := Position + Circle.Left;
    Result.Right := Result.Left + 1;
    Result.Top := Mid - Wid;
    Result.Bottom := Mid + Wid;
  end;

var
  c1, c2, c: TRGB;  //for easy access to RGB values as well as TColor value
  x, y: integer;         //current pixel position to be set
  OldPenWidth: integer;  //Save old settings to restore them properly
  OldPenStyle: TPenStyle;//see above
begin
  c1.Color := ColorToRGB(Color1);  //convert system colors to RGB values
  c2.Color := ColorToRGB(Color2);  //if neccessary
  OldPenWidth := Canvas.Pen.Width; //get old settings
  OldPenStyle := Canvas.Pen.Style;
  Canvas.Pen.Width := 1;             //ensure correct pen settings
  Canvas.Pen.Style := psInsideFrame;

  case Vert of
    True:
    begin
      for y := 0 to ARect.Bottom - ARect.Top do
      begin
        c.r := Round(c1.r + (c2.r - c1.r) * y / (ARect.Bottom - ARect.Top));
        c.g := Round(c1.g + (c2.g - c1.g) * y / (ARect.Bottom - ARect.Top));
        c.b := Round(c1.b + (c2.b - c1.b) * y / (ARect.Bottom - ARect.Top));
        Canvas.Brush.Color := c.Color;
        Canvas.FillRect(GetRectV(ARect, y));
      end;
    end;
    False:
    begin
      for x := 0 to ARect.Right - ARect.Left do
      begin
        c.r := Round(c1.r + (c2.r - c1.r) * x / (ARect.Right - ARect.Left));
        c.g := Round(c1.g + (c2.g - c1.g) * x / (ARect.Right - ARect.Left));
        c.b := Round(c1.b + (c2.b - c1.b) * x / (ARect.Right - ARect.Left));
        Canvas.Brush.Color := c.Color;
        Canvas.FillRect(GetRectH(ARect, x));
      end;
    end;
  end;
  Canvas.Pen.Width := OldPenWidth; //restore old settings
  Canvas.Pen.Style := OldPenStyle;
end;

function RGBtoHSV(Col: TColor): THSV;
begin
  ColorToHLS(Col, Result.h, Result.L, Result.S);
end;

function HSVtoRGB(HSV: THSV): TColor;
begin
  Result:=HLStoColor(HSV.H, HSV.L, HSV.S);
end;

function BlackNWhite(c: TColor): TColor;
var
  H: THSV;
begin
  H := RGBtoHSV(c);
  H.S := 0;
  Result := HSVtoRGB(H);
end;

procedure DrawTextCenter(c: TCanvas; Width, Height: integer; Caption: string);
begin
  c.TextOut((Width div 2) - (c.TextWidth(Caption) div 2),
    (Height div 2) - (c.TextHeight(Caption) div 2), Caption);
end;

function LightenColor(c: TColor; Val: TPercent): TColor;
var
  h: THSV;
  o: Byte;
begin
  h := RGBtoHSV(ColorToRGB(c));
  o:=h.V;
  h.v := h.v + h.v div 100 * Val;
  if h.v<o then
    h.v:=255;
  Result := HSVtoRGB(h);
end;

function DarkenColor(c: TColor; Val: TPercent): TColor;
var
  h: THSV;
  o: Byte;
begin
  h := RGBtoHSV(ColorToRGB(c));
  o:=h.V;
  h.v := h.v - h.v div 100 * Val;
  if h.v>o then
    h.v:=0;
  Result := HSVtoRGB(h);
end;

end.
