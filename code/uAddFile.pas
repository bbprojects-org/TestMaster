{ ==============================================================================

  ADD FILE FORM - get filename dialog box

    Interface:
    - Filename; property returns the user's chosen filename


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

unit uAddFile;

{$mode objfpc}{$h+}

interface

uses
  Forms, StdCtrls, ExtCtrls, Classes,
  //
  uCommon;

type

  { TGetFilename }

  TGetFilename = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    edFilename: TLabeledEdit;
    procedure edFilenameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetFilename: string;
  public
    property Filename: string read GetFilename;
  end;

var
  GetFilename: TGetFilename;


implementation

{$R *.lfm}

{ SHOW }

procedure TGetFilename.FormShow(Sender: TObject);
begin
  SwapButtons(btnCancel, btnOk);
end;


{ EDIT FILENME CHANGE }

procedure TGetFilename.edFilenameChange(Sender: TObject);
begin
  btnOk.Enabled := (Length(edFilename.Text) > 0)
end;


{ PROPERTY GETTER FILENAME }

function TGetFilename.GetFilename: string;
var
  DotPos: integer;
begin
  DotPos := Pos('.', edFilename.Text);       // Remove any ext
  if (DotPos > 0) then
    edFilename.Text := Copy(edFilename.Text, 1, DotPos-1);
  Result := edFilename.Text;
end;


end.
