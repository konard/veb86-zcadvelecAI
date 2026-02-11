
program test_polyface_load;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  uzeffdxfsupport,
  uzeentpolyfacemesh,
  uzedrawingdef,
  uzclog;

var
  drawing: TDrawingDef;
  context: TIODXFLoadContext;
  reader: TZMemReader;
  polyfacemesh: GDBObjPolyFaceMesh;
  stream: TMemoryStream;
  dxf_data: TStringList;

begin
  try
    programlog := TProgramLog.Create(nil);
    programlog.LogOutFormatStr('Начало теста загрузки PolyFaceMesh', [], LM_Info);
    
    // Загружаем DXF файл
    dxf_data := TStringList.Create;
    dxf_data.LoadFromFile('/tmp/gh-issue-solver-1770837406897/cad_source/polyface.dxf');
    
    stream := TMemoryStream.Create;
    stream.Write(dxf_data.GetText[0], Length(dxf_data.GetText));
    stream.Position := 0;
    
    reader := TZMemReader.Create(stream);
    
    drawing := TDrawingDef.Create;
    context.GDBVertexLoadCache.init(1000);
    
    // Создаем PolyFaceMesh
    polyfacemesh.init(nil, nil, 0);
    
    // Загружаем из DXF
    polyfacemesh.LoadFromDXF(reader, nil, drawing, context);
    
    programlog.LogOutFormatStr('Результат: %d вершин, %d граней', 
      [polyfacemesh.GetVertexCount, polyfacemesh.GetFaceCount], LM_Info);
    
    // Проверяем ожидаемые значения
    if (polyfacemesh.GetVertexCount = 4) and (polyfacemesh.GetFaceCount = 1) then
      programlog.LogOutFormatStr('ТЕСТ ПРОЙДЕН: Корректные количества вершин и граней', [], LM_Info)
    else
      programlog.LogOutFormatStr('ТЕСТ НЕ ПРОЙДЕН: Ожидается 4 вершины и 1 грань, получено %d вершин и %d граней', 
        [polyfacemesh.GetVertexCount, polyfacemesh.GetFaceCount], LM_Info);
    
  except
    on E: Exception do
      programlog.LogOutFormatStr('ИСКЛЮЧЕНИЕ: %s', [E.Message], LM_Info);
  end;
  
  programlog.Free;
end.
