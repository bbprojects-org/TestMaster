{ ==============================================================================

  OPTIONS FORM

    Dialog form to select various administrator options; enable various
    buttons, running score, test screen colour, etc

    Interface:
    - Various options can be read via properties


  LICENSE:

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

  =============================================================================}

unit uOptionsForm;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Graphics,
  //
  uGetNewPasswordForm, uCommon;

type
  
  { TAdminOptions }

  TAdminOptions = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    cbDeletePermitted: TCheckBox;
    cbEnableAnswers: TCheckBox;
    cbAllowEdits: TCheckBox;
    btnScreenColor: TButton;
    btnChangePassword: TButton;
    ColorDialog1: TColorDialog;
    panelColour: TPanel;
    cbEnableClearResults: TCheckBox;
    cbEnableRunningScore: TCheckBox;
    btnDefault: TButton;
    procedure btnScreenColorClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure cbAllowEditsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnChangePasswordClick(Sender: TObject);
  private
    function GetAddEditEnabled: boolean;
    function GetAnswersEnabled: boolean;
    function GetClearResultsEnabled: boolean;
    function GetDeleteEnabled: boolean;
    function GetPanelColor: TColor;
    function GetRunningScoreEnabled: boolean;
    procedure SetAddEditEnabled(aValue: boolean);
    procedure SetAnswersEnabled(aValue: boolean);
    procedure SetClearResultsEnabled(aValue: boolean);
    procedure SetDeleteEnabled(aValue: boolean);
    procedure SetPanelColor(aValue: TColor);
    procedure SetRunningScoreEnabled(aValue: boolean);
  public
    property IsAnswersEnabled: boolean read GetAnswersEnabled write SetAnswersEnabled;
    property IsClearResultsEnabled: boolean read GetClearResultsEnabled write SetClearResultsEnabled;
    property IsRunningScoreEnabled: boolean read GetRunningScoreEnabled write SetRunningScoreEnabled;
    property IsAddEditEnabled: boolean read GetAddEditEnabled write SetAddEditEnabled;
    property IsDeleteEnabled: boolean read GetDeleteEnabled write SetDeleteEnabled;
    property PanelColor: TColor read GetPanelColor write SetPanelColor;
  end;

var
  AdminOptions: TAdminOptions;


implementation

uses
  uMainForm;

{$R *.lfm}

{ SHOW }

procedure TAdminOptions.FormShow(Sender: TObject);
begin
  SwapButtons(btnCancel, btnOk);
  ShowHint := AppShowHint;
  cbDeletePermitted.Enabled := cbAllowEdits.Checked;
end;


{ BUTTON SCREEN COLOUR }

procedure TAdminOptions.btnScreenColorClick(Sender: TObject);
begin
  ColorDialog1.Color := panelColour.Color;
  ColorDialog1.Execute;
  panelColour.Color := ColorDialog1.Color;
end;  


{ BUTTON CHANGE PASSWORD }

procedure TAdminOptions.btnChangePasswordClick(Sender: TObject);
var
  NewPasswordDlg: TNewPasswordDlg;
begin
  NewPasswordDlg := TNewPasswordDlg.Create(self);
  try
    if (NewPasswordDlg.ShowModal = mrOk) then
      MainForm.WriteStatus;
  finally
    NewPasswordDlg.Free;
  end;
end;


{ BUTTON DEFAULT }

procedure TAdminOptions.btnDefaultClick(Sender: TObject);
begin
  cbEnableAnswers.Checked := False;
  cbEnableClearResults.Checked := False;
  cbAllowEdits.Checked := False;
  cbDeletePermitted.Checked := False;
  cbEnableRunningScore.Checked := False;
  panelColour.Color := clDefault;
end;


{ GET / SET PROPERTY VALUES }

procedure TAdminOptions.cbAllowEditsClick(Sender: TObject);
begin
  cbDeletePermitted.Enabled := cbAllowEdits.Checked;
end;


function TAdminOptions.GetAnswersEnabled: boolean;
begin
  Result := cbEnableAnswers.Checked;
end;

procedure TAdminOptions.SetAnswersEnabled(aValue: boolean);
begin
  cbEnableAnswers.Checked := aValue;
end;


function TAdminOptions.GetClearResultsEnabled: boolean;
begin
  Result := cbEnableClearResults.Checked;
end;


procedure TAdminOptions.SetClearResultsEnabled(aValue: boolean);
begin
  cbEnableClearResults.Checked := aValue;
end;


function TAdminOptions.GetRunningScoreEnabled: boolean;
begin
  Result := cbEnableRunningScore.Checked;
end;

procedure TAdminOptions.SetRunningScoreEnabled(aValue: boolean);
begin
  cbEnableRunningScore.Checked := aValue;
end;


function TAdminOptions.GetAddEditEnabled: boolean;
begin
  Result := cbAllowEdits.Checked;
end;

procedure TAdminOptions.SetAddEditEnabled(aValue: boolean);
begin
  cbAllowEdits.Checked := aValue;
end;


function TAdminOptions.GetDeleteEnabled: boolean;
begin
  Result := cbDeletePermitted.Checked;
end;

procedure TAdminOptions.SetDeleteEnabled(aValue: boolean);
begin
  cbDeletePermitted.Checked := aValue;
end;


function TAdminOptions.GetPanelColor: TColor;
begin
  Result := panelColour.Color;
end;

procedure TAdminOptions.SetPanelColor(aValue: TColor);
begin
  panelColour.Color :=  aValue;
end;


end.
