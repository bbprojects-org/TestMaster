{ ==============================================================================

  RESULTS FORM

    This form shows an optional summary of the results of test taken

    Interface:
    - Add method; writes line of text to the results memo (and associated file)


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

unit uResultsForm;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  //
  uOptionsForm, uJsonCfgFile, uCommon;

type
  
  { TResultsForm }

  TResultsForm = class(TForm)
    btnClose: TButton;
    btnClear: TButton;
    memoResults: TMemo;
    procedure btnClearClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure memoResultsChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Initialise: boolean;
    ResultsFile: string;
    IsModified: boolean;                // Added as TMemo.Modified not working?
  public
    procedure Add(aLine: string);
  end;

var
  ResultsForm: TResultsForm;


implementation

{$R *.lfm}

const
  SECT_CFG = 'ResultsForm';


{ CREATE }

procedure TResultsForm.FormCreate(Sender: TObject);
begin                                      
  Left := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_LEFT, 40);
  Top := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_TOP, 40);
  Width := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_WIDTH, 0);
  Height := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_HEIGHT, 0);
  //
  ResultsFile := GetAppDataDirectory(adUser) + 'results.txt';
  Initialise := True;
  if (FileExists(ResultsFile)) then
    memoResults.Lines.LoadFromFile(ResultsFile)
  else
    memoResults.Lines.Clear;
  Initialise := False;
  IsModified := False;
end;


{ DESTROY  }

procedure TResultsForm.FormDestroy(Sender: TObject);
begin     
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_LEFT, Left);
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_TOP, Top);
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_WIDTH, Width);
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_HEIGHT, Height);
  //
  if (IsModified) then
    memoResults.Lines.SaveToFile(ResultsFile);
end;


{ SHOW }

procedure TResultsForm.FormShow(Sender: TObject);
begin
  Font.Size := AppFontSize;
  ShowHint := AppShowHint;
  btnClear.Visible := AdminOptions.IsClearResultsEnabled;
  btnClear.Enabled := (memoResults.Text <> '');
end;


{ BUTTON CLEAR }

procedure TResultsForm.btnClearClick(Sender: TObject);
begin
  if (MessageQuery('Clear Results Summary', 'Are you sure you want to delete this summary?')) then
    begin
      memoResults.Lines.Clear;
      if (FileExists(ResultsFile)) then
        DeleteFile(ResultsFile);
    end;
end;


{ RESULTS MEMO CHANGE }

procedure TResultsForm.memoResultsChange(Sender: TObject);
begin
  btnClear.Enabled := (memoResults.Text <> '');
end;


{ CLOSE }

procedure TResultsForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


{ ADD METHOD }

procedure TResultsForm.Add(aLine: string);
begin
  memoResults.Lines.Add(aLine);
  IsModified := True;
end;


end.
