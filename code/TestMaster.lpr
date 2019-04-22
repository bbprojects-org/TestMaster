program TestMaster;

{$mode objfpc}{$h+}

uses
  Forms,
  SysUtils,
  Dialogs, Interfaces,
  uRunTestForm in 'uRunTestForm.pas',
  uAddEditForm in 'uAddEditFor.pas',
  uSearchForm in 'uSearchForm.pas',
  uSelectExhibitForm in 'uExhibitsForm.pas',
  uShowExhibitForm in 'uShowForm.pas',
  uMainForm in 'MainForm.pas',
  uAboutForm in 'uAboutForm.pas',
  uResultsForm in 'uResultsForm.pas',
  uGetNewPasswordForm in 'uGetNewPasswordForm.pas',
  uGetPasswordForm in 'uGetPasswordForm.pas',
  uOptionsForm in 'uOptionsForm.pas',
  uTimerValueForm in 'uTimerValueForm.pas',
  uCommon;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TResultsForm, ResultsForm);
  Application.CreateForm(TAdminOptions, AdminOptions);
  Application.Run;
end.
