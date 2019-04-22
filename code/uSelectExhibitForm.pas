{ ==============================================================================

  SELECT EXHIBIT FORM

    Interface:                                                      
    - Filepath; property sets the file path to the images (normally Tests folder)
    - Filename; property returns the chosen exhibit's filename


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

unit uSelectExhibitForm;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, FileCtrl, StdCtrls, ExtCtrls,
  //
  uJsonCfgFile, uCommon;

type

  { TExhibitManager }

  TExhibitManager = class(TForm)
    btnSelect: TButton;
    btnCancel: TButton;
    FileListBox1: TFileListBox;
    gbFileList: TGroupBox;
    gbImage: TGroupBox;
    Image1: TImage;
    lblWarning: TLabel;
    panelWarning: TPanel;
    procedure FileListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FileListBox1DblClick(Sender: TObject);
  private
    fFilePath: string;
    fFilename: string;
    ImageLoaded: string;
    function GetFilename: string;
    procedure ShowImage;
  public
    property FilePath: string read fFilePath write fFilePath;
    property Filename: string read GetFilename write fFilename;
  end;


implementation

{$R *.lfm}     

const
  SECT_CFG = 'SelectExhibitForm';


{ CREATE }

procedure TExhibitManager.FormCreate(Sender: TObject);
begin                                           
  Left := AppCfg.ReadInteger(SECT_CFG, INI_WDW_LEFT, 40);
  Top := AppCfg.ReadInteger(SECT_CFG, INI_WDW_TOP, 40);
  Width := AppCfg.ReadInteger(SECT_CFG, INI_WDW_WIDTH, 0);
  Height := AppCfg.ReadInteger(SECT_CFG, INI_WDW_HEIGHT, 0);
  //
  fFilename :=  '';
  ImageLoaded :=  '';
end;


{ DESTROY }

procedure TExhibitManager.FormDestroy(Sender: TObject);
begin   
  AppCfg.WriteInteger(SECT_CFG, INI_WDW_LEFT, Left);
  AppCfg.WriteInteger(SECT_CFG, INI_WDW_TOP, Top);
  AppCfg.WriteInteger(SECT_CFG, INI_WDW_WIDTH, Width);
  AppCfg.WriteInteger(SECT_CFG, INI_WDW_HEIGHT, Height);
end;


{ SHOW }

procedure TExhibitManager.FormShow(Sender: TObject);
var
  ThisIndex:  integer;
begin               
  SwapButtons(btnCancel, btnSelect);
  lblWarning.Caption := Format('WARNING - No images can be found in the "%s" directory for use as exhibits',  [fFilePath]);
  panelWarning.Visible := False;
  btnSelect.Enabled := True;
  FileListBox1.Directory := fFilePath;
  if (FileListBox1.Items.Count = 0) then
    begin
      panelWarning.Visible := True;
      btnSelect.Enabled := False;
    end
  else
    begin
      FileListBox1.ItemIndex := 0;      // Select first item
      if (fFilename <>  '') then
        begin
          ThisIndex := FileListBox1.Items.IndexOf(fFilename);
          if (ThisIndex <>  -1) then
            FileListBox1.ItemIndex := ThisIndex;
        end;
      ShowImage;
    end;
  FormResize(nil);
end;


{ FILE LISTBOX CLICK / DOUBLE CLICK }

procedure TExhibitManager.FileListBox1Click(Sender: TObject);
begin
  ShowImage;
end;


{ SHOW IMAGE }

procedure TExhibitManager.ShowImage;
begin
  if (FileListBox1.FileName = '') then Exit;

  if (FileListBox1.FileName <> ImageLoaded) then
    begin
      Image1.Picture.LoadFromFile(FileListBox1.FileName);
      ImageLoaded := FileListBox1.FileName;
    end;

  if (Image1.Picture.Width > Image1.Width) or (Image1.Picture.Height > Image1.Height) then
    begin
      Image1.Stretch := True;
      Image1.Proportional := True;
    end
  else
    begin
      Image1.Stretch := False;
      Image1.Proportional := False;
    end;
end;


procedure TExhibitManager.FileListBox1DblClick(Sender: TObject);
begin
  ModalResult := mrOk;                  // Exit the form
end;


{ PROPERTY GET / SET }

function TExhibitManager.GetFilename: string;
begin
  Result := FileListBox1.FileName;
end;  


{ RESIZE }

procedure TExhibitManager.FormResize(Sender: TObject);
var
  UnitWidth: integer;
begin
  UnitWidth := trunc((Width - 12) / 3);
  if (UnitWidth < 300) then
    gbImage.Width := UnitWidth * 2
  else
    gbImage.Width := Width - 300 - 12;
  panelWarning.Width := trunc(gbImage.Width * 0.9);
  panelwarning.Left := gbImage.Left + (gbImage.Width - panelWarning.Width) div 2;
  ShowImage;
end;


end.
