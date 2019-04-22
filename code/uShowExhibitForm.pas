{ ==============================================================================

  SHOW EXHIBIT FORM

    Show the exhibit at actual size

    Interface:
    - ExhibitFile; full filename of the image to show


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

unit uShowExhibitForm;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Classes, Controls, Forms, ExtCtrls, StdCtrls,
  //
  uJsonCfgFile, uCommon;

type

  { TShowExhibitForm }

  TShowExhibitForm = class(TForm)
    imgExhibit: TImage;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    fFilename: string;        
    ImageLoaded: string;
    procedure ShowImage;
  public
    property ExhibitFile: string write fFilename;
  end;


implementation

{$R *.lfm}

const
  SECT_CFG = 'ShowExhibitForm';


{ CREATE }

procedure TShowExhibitForm.FormCreate(Sender: TObject);
begin
  Left := AppCfg.ReadInteger(SECT_CFG, INI_WDW_LEFT, 40);
  Top := AppCfg.ReadInteger(SECT_CFG, INI_WDW_TOP, 40);
  Width := AppCfg.ReadInteger(SECT_CFG, INI_WDW_WIDTH, 600);
  Height := AppCfg.ReadInteger(SECT_CFG, INI_WDW_HEIGHT, 400);
  //
  ImageLoaded := '';
end;


{ DESTROY }

procedure TShowExhibitForm.FormDestroy(Sender: TObject);
begin                                       
  AppCfg.WriteInteger(SECT_CFG, INI_WDW_LEFT, Left);
  AppCfg.WriteInteger(SECT_CFG, INI_WDW_TOP, Top);
  AppCfg.WriteInteger(SECT_CFG, INI_WDW_WIDTH, Width);
  AppCfg.WriteInteger(SECT_CFG, INI_WDW_HEIGHT, Height);
end;


{ SHOW }

procedure TShowExhibitForm.FormShow(Sender: TObject);
begin
  ShowImage;
end;


procedure TShowExhibitForm.ShowImage;
begin
  if (fFilename <> ImageLoaded) then
    begin
      imgExhibit.Picture.LoadFromFile(fFilename);
      ImageLoaded := fFilename;
    end;

  FitImage(imgExhibit);
end;


{ BUTTON CLOSE }

procedure TShowExhibitForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


{ RESIZE }

procedure TShowExhibitForm.FormResize(Sender: TObject);
begin
  ShowImage;
end;


end.
