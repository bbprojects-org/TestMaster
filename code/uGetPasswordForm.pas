{ ==============================================================================

  GET PASSWORD FORM


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

unit uGetPasswordForm;

{$mode objfpc}{$h+}

interface

uses
  Classes, Forms, Controls, StdCtrls, Buttons, SysUtils, Dialogs,
  //
  uCommon;

type
  TGetPasswordDlg = class(TForm)
    Label1: TLabel;
    EditPassword: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;


implementation

{$R *.lfm}

procedure TGetPasswordDlg.FormShow(Sender: TObject);
begin
  SwapButtons(btnOk, btnCancel);
  ShowHint := AppShowHint;
  EditPassword.Text := '';
  EditPassword.Setfocus;
end;


procedure TGetPasswordDlg.btnOkClick(Sender: TObject);
begin
  if (Scramble(uppercase(EditPassword.Text)) <> AppPassword) then
    begin
      MessageWarning('Get Password', 'Password incorrect');
      EditPassword.Setfocus;
      EditPassword.SelectAll;
      ModalResult := mrNone;            //  Do not exit dialog
    end;
end;


end.

