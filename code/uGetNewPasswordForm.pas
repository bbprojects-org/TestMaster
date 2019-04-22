{ ==============================================================================

  GET NEW PASSWORD FORM

    A dialog box to allow the user to enter a new administrator password
    (with confirmation)


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

unit uGetNewPasswordForm;

{$mode objfpc}{$h+}

interface

uses
  Classes, Forms, Controls, StdCtrls, Buttons, Dialogs, ExtCtrls, SysUtils,
  //
  uCommon;

type

  { TNewPasswordDlg }

  TNewPasswordDlg = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    edConfirmPassword: TLabeledEdit;
    edNewPassword: TLabeledEdit;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;


implementation

{$R *.lfm}

{ CREATE }

procedure TNewPasswordDlg.FormCreate(Sender: TObject);
begin
  SwapButtons(btnCancel, btnOk);
end;


{  SHOW }

procedure TNewPasswordDlg.FormShow(Sender: TObject);
begin
  ShowHint := AppShowHint;
  edNewPassword.Text := '';
  edConfirmPassword.Text := '';
  edNewPassword.SetFocus;
end;


{ BUTTON OK }

procedure TNewPasswordDlg.btnOkClick(Sender: TObject);
begin
  ModalResult := mrNone;                // Prevent exit unless password OK
  if (edNewPassword.Text = edConfirmPassword.Text) then
    begin
      // Do not allow blank password
      AppPassword := Scramble(UpperCase(edNewPassword.Text));
      MessageInfo('Password changed');
      ModalResult := mrOk;
    end
  else
    begin
      MessageWarning('Change Password', 'New password and confirmation differ');
      edNewPassword.Text := '';
      edConfirmPassword.Text := '';
      edNewPassword.Setfocus;
    end;
end;


end.
 
