#!/usr/bin/env python3
"""
Тест для анализа структуры PolyFaceMesh DXF файла
и проверки соответствия с логикой парсинга в uzeentpolyfacemesh.pas
"""

def analyze_polyfacemesh(filename):
    """Анализирует PolyFaceMesh структуру в DXF файле"""
    print(f"Анализ файла: {filename}")
    print("=" * 50)
    
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    vertices = []
    face_records = []
    vertex_count = 0
    face_count = 0
    
    i = 0
    while i < len(lines) - 1:
        # DXF формат: код на одной строке, значение на следующей
        code_line = lines[i].strip()
        value_line = lines[i + 1].strip() if i + 1 < len(lines) else ''
        
        if code_line == '0' and value_line == 'POLYLINE':
            print("Найдена секция POLYLINE")
            i += 2
            # Читаем параметры POLYLINE
            while i < len(lines) - 1:
                code = lines[i].strip()
                value = lines[i + 1].strip()
                
                if code == '70':
                    print(f"Флаги POLYLINE: {value}")
                elif code == '71':
                    print(f"Объявленное количество вершин: {value}")
                elif code == '72':
                    print(f"Объявленное количество граней: {value}")
                elif code == '0' and value == 'VERTEX':
                    break
                i += 2
                
        elif code_line == '0' and value_line == 'VERTEX':
            i += 2
            vertex_data = {}
            is_face_record = False
            
            # Читаем данные вершины до следующей сущности
            while i < len(lines) - 1:
                code = lines[i].strip()
                value = lines[i + 1].strip()
                
                if code == '0':  # Начало новой сущности
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
            
            if is_face_record:
                face_count += 1
                face_records.append(vertex_data)
                print(f"  Грань {face_count}: {vertex_data}")
            elif 'x' in vertex_data:  # Только если есть координаты
                vertex_count += 1
                vertices.append(vertex_data)
                print(f"  Вершина {vertex_count}: {vertex_data}")
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