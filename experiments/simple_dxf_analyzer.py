#!/usr/bin/env python3
"""
Анализатор PolyFaceMesh из DXF файла
"""
import re

def analyze_dxf_file(file_path):
    """Анализирует DXF файл и извлекает информацию о PolyFaceMesh"""
    print(f"Анализ файла: {file_path}")
    
    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Находим секцию ENTITIES
    entities_match = re.search(r'0\s+SECTION\s+2\s+ENTITIES(.*?)(?=0\s+ENDSEC)', content, re.DOTALL)
    if not entities_match:
        print("Секция ENTITIES не найдена")
        return
    
    entities_section = entities_match.group(1)
    print("Найдена секция ENTITIES")
    
    # Находим POLYLINE запись
    polyline_match = re.search(r'0\s+POLYLINE(.*?)(?=0\s+(SEQEND|POLYLINE|ENDSEC))', entities_section, re.DOTALL)
    if not polyline_match:
        print("Запись POLYLINE не найдена")
        return
    
    polyline_data = polyline_match.group(1)
    print("Найдена запись POLYLINE:")
    
    # Извлекаем флаги и счетчики из POLYLINE
    flags = re.findall(r'70\s+(\d+)', polyline_data)
    m_count = re.findall(r'71\s+(\d+)', polyline_data)
    n_count = re.findall(r'72\s+(\d+)', polyline_data)
    
    print(f"  Флаги: {flags}")
    print(f"  Количество вершин (71): {m_count}")
    print(f"  Количество граней (72): {n_count}")
    
    # Находим все VERTEX записи
    vertex_pattern = r'0\s+VERTEX(.*?)(?=0\s+(VERTEX|SEQEND|ENDSEC))'
    vertices = re.finditer(vertex_pattern, entities_section, re.DOTALL)
    
    vertex_coords = []
    face_indices = []
    
    for i, vertex_match in enumerate(vertices):
        vertex_data = vertex_match.group(1)
        print(f"\nVERTEX #{i+1}:")
        
        # Определяем тип вершины по флагу 70
        flag_match = re.search(r'70\s+(\d+)', vertex_data)
        if flag_match:
            flag = int(flag_match.group(1))
            print(f"  Флаг: {flag}")
            
            if flag & 128:  # Face record
                print("  Тип: Запись грани (Face Record)")
                
                # Извлекаем индексы вершин
                indices = []
                for code in [71, 72, 73, 74]:
                    index_match = re.search(f'{code}\\s+(\\d+)', vertex_data)
                    if index_match:
                        indices.append(int(index_match.group(1)))
                    else:
                        indices.append(0)
                
                face_indices.append(indices)
                print(f"  Индексы вершин: {indices}")
                
            else:  # Vertex с координатами
                print("  Тип: Вершина с координатами")
                
                # Извлекаем координаты
                x_match = re.search(r'10\s+([-\d.]+)', vertex_data)
                y_match = re.search(r'20\s+([-\d.]+)', vertex_data)
                z_match = re.search(r'30\s+([-\d.]+)', vertex_data)
                
                if x_match and y_match and z_match:
                    x = float(x_match.group(1))
                    y = float(y_match.group(1))
                    z = float(z_match.group(1))
                    vertex_coords.append((x, y, z))
                    print(f"  Координаты: ({x:.6f}, {y:.6f}, {z:.6f})")
                else:
                    print("  Ошибка: не найдены координаты")
        
        # Проверяем подтип
        subtype_match = re.search(r'100\s+(AcDbPolyFaceMeshVertex|AcDbFaceRecord)', vertex_data)
        if subtype_match:
            print(f"  Подтип: {subtype_match.group(1)}")
    
    print(f"\n=== ИТОГОВЫЙ АНАЛИЗ ===")
    print(f"Вершин с координатами: {len(vertex_coords)}")
    print(f"Записей граней: {len(face_indices)}")
    
    print("\nКоординаты вершин:")
    for i, (x, y, z) in enumerate(vertex_coords):
        print(f"  Вершина {i+1}: ({x:.6f}, {y:.6f}, {z:.6f})")
    
    print("\nИндексы граней:")
    for i, indices in enumerate(face_indices):
        print(f"  Грань {i+1}: {indices}")
    
    # Проверяем корректность индексов
    print("\nПроверка корректности индексов:")
    for i, indices in enumerate(face_indices):
        for j, idx in enumerate(indices):
            if idx > 0 and idx > len(vertex_coords):
                print(f"  ОШИБКА: Грань {i+1}, индекс {j+1}: {idx} > количество вершин {len(vertex_coords)}")
    
    return vertex_coords, face_indices

if __name__ == "__main__":
    file_path = "/tmp/gh-issue-solver-1770837406897/cad_source/polyface.dxf"
    analyze_dxf_file(file_path)