unit MinimalSwitch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TMinimalSwitch = class(TGraphicControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I customswitch_icon.lrs}
  RegisterComponents('Minimal Components',[TMinimalSwitch]);
end;

end.
