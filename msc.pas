{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MSC;

interface

uses
  MinimalButton, DrawUtils, MinimalProgressBar, MinimalTrackBar, MinimalPanel, 
  MinimalSpeedButton, MinimalCheckbox, MinimalCheckboxGroup, DataGraph, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MinimalButton', @MinimalButton.Register);
  RegisterUnit('MinimalProgressBar', @MinimalProgressBar.Register);
  RegisterUnit('MinimalTrackBar', @MinimalTrackBar.Register);
  RegisterUnit('MinimalPanel', @MinimalPanel.Register);
  RegisterUnit('MinimalSpeedButton', @MinimalSpeedButton.Register);
  RegisterUnit('MinimalCheckbox', @MinimalCheckbox.Register);
  RegisterUnit('MinimalCheckboxGroup', @MinimalCheckboxGroup.Register);
  RegisterUnit('DataGraph', @DataGraph.Register);
end;

initialization
  RegisterPackage('MSC', @Register);
end.
