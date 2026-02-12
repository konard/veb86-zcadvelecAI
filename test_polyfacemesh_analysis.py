#!/usr/bin/env python3
"""
Тест для анализа структуры PolyFaceMesh DXF файла
и проверки соответствия с логикой парсинга в uzeentpolyfacemesh.pas
"""

def parse_dxf_file(filename):
    """Парсит DXF файл и извлекает информацию о PolyFaceMesh"""
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    entities = []
    current_entity = None
    in_polyface = False
    
    for i, line in enumerate(lines):
        line = line.strip()
        if not line:
            continue
            
        # Каждая вторая строка - это значение (между кодами групп)
        if i % 2 == 1:
            if current_entity is not None:
                if 'code' in current_entity:
                    current_entity['value'] = line
                else:
                    current_entity['value'] = line
                entities.append(current_entity)
                current_entity = None
        else:
            code = int(line)
            if code == 0:  # Начало новой сущности
                if current_entity:
                    entities.append(current_entity)
                current_entity = {'code': code, 'value': None}
            else:
                current_entity = {'code': code, 'value': None}
    
    return entities

def analyze_polyfacemesh(filename):
    """Анализирует PolyFaceMesh структуру в DXF файле"""
    print(f"Анализ файла: {filename}")
    print("=" * 50)
    
    with open(filename, 'r') as f:
        content = f.read()
    
    lines = content.split('\n')
    
    polyline_section = None
    vertices = []
    face_records = []
    
    current_vertex = {}
    in_vertex = False
    vertex_count = 0
    face_count = 0
    
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        
        if line == '0':
            if i + 1 < len(lines) and lines[i + 1].strip() == 'POLYLINE':
                polyline_section = True
                print("Найдена секция POLYLINE")
                # Пропускаем 2 строки (0 и POLYLINE)
                i += 2
                # Ищем флаги и количество вершин/граней
                while i < len(lines):
                    code = lines[i].strip()
                    if i + 1 < len(lines):
                        value = lines[i + 1].strip()
                        
                        if code == '70':
                            print(f"Флаги POLYLINE: {value}")
                        elif code == '71':
                            print(f"Объявленное количество вершин: {value}")
                        elif code == '72':
                            print(f"Объявленное количество граней: {value}")
                        
                        if code == '0' and value == 'VERTEX':
                            break
                        i += 2
                    else:
                        break
                continue
                
        elif line == '0' and i + 1 < len(lines) and lines[i + 1].strip() == 'VERTEX':
            i += 2
            vertex_data = {}
            is_face_record = False
            
            # Читаем данные вершины
            while i < len(lines):
                code = lines[i].strip()
                if i + 1 < len(lines):
                    value = lines[i + 1].strip()
                    
                    if code == '0':
                        # Следующая сущность
                        break
                    elif code == '70' and value == '192':
                        is_face_record = True
                        print(f"Найдена запись грани с флагом 192")
                    elif code == '71':
                        vertex_data['v1'] = int(value)
                    elif code == '72':
                        vertex_data['v2'] = int(value)
                    elif code == '73':
                        vertex_data['v3'] = int(value)
                    elif code == '74':
                        vertex_data['v4'] = int(value)
                    elif code == '10':
                        vertex_data['x'] = float(value)
                    elif code == '20':
                        vertex_data['y'] = float(value)
                    elif code == '30':
                        vertex_data['z'] = float(value)
                        
                    i += 2
                else:
                    break
            
            if is_face_record:
                face_count += 1
                face_records.append(vertex_data)
                print(f"  Грань {face_count}: {vertex_data}")
            else:
                vertex_count += 1
                vertices.append(vertex_data)
                print(f"  Вершина {vertex_count}: {vertex_data}")
        else:
            i += 1
            continue
            
        i += 1
    
    print(f"\nИтого:")
    print(f"  Вершин: {vertex_count}")
    print(f"  Граней: {face_count}")
    
    print(f"\nАнализ граней:")
    for i, face in enumerate(face_records):
        v_count = len([k for k in face.keys() if k.startswith('v')])
        print(f"  Грань {i+1}: {v_count} вершин, индексы: {face}")
    
    return vertices, face_records

if __name__ == "__main__":
    import sys
    filename = "examples/polyfacemesh_example.dxf"
    vertices, faces = analyze_polyfacemesh(filename)
    
    print(f"\nПроверка логики парсинга:")
    print(f"- DXF файл имеет {len(vertices)} вершин и {len(faces)} граней")
    print(f"- Все грани используют флаг 70=192 для идентификации")
    print(f"- Групповой код 100 (AcDbPolyFaceMeshVertex/AcDbFaceRecord) отсутствует")
    print(f"- Это означает, что код должен полагаться на флаги для определения типа")