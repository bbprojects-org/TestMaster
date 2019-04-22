{ ==============================================================================

  ABOUT FORM

    A generic about form showing the app version and copyright. It also
    promotes the Lazarus / Free Pascal programming tools, and their versions
    used


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

unit uAboutForm;

{$mode objfpc}{$h+}

interface

uses
  Classes, Forms, Controls, StdCtrls, Buttons, ExtCtrls, SysUtils,
  //
  uCommon;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    btnOk: TButton;
    Image1: TImage;
    lblFPCVer: TLabel;
    lblLazarusVer: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    LogoImage: TImage;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

const
  APP_VERSION = '4.02';
  APP_COPYRIGHT = 'Copyright RC Beveridge, 1998-2019';


implementation

{$R *.lfm}

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  lblVersion.Caption := Format('Version %s (%s)', [APP_VERSION, {$I %DATE%}]);
  lblCopyright.Caption := APP_COPYRIGHT;
  lblLazarusVer.Caption := 'Lazarus: ' + LCLVersion;
  lblFPCVer.Caption := 'FPC: ' + {$I %FPCVERSION%};
  {$ifdef darwin}
    btnOk.Visible := False;             // On MacOS generally no exit button
  {$endif}
end;


end.


