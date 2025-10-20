# Issue #279: Enable GDI System Rendering for SHX Fonts

## Problem Description

When GDI rendering mode was enabled, SHX font symbols were clustering near the origin (0,0,0) instead of being positioned at their correct locations.

## Root Cause Analysis

The ZCAD rendering system has two rendering paths:
1. **ZGL Rendering**: Draws text using triangles (vector geometry)
2. **GDI System Rendering**: Uses the OS native GDI API with ExtTextOut

The choice between these paths is controlled by the `IsCanSystemDraw()` method in font implementations.

### Before the Fix

In `uzefontshx.pas:39`, the SHX font implementation had:
```pascal
function TZESHXFontImpl.IsCanSystemDraw:Boolean;
begin
  result:=false;  // SHX fonts used ZGL rendering only
end;
```

This meant:
- **TTF fonts**: `IsCanSystemDraw()` returns `true` → Uses GDI rendering ✓
- **SHX fonts**: `IsCanSystemDraw()` returns `false` → Uses ZGL rendering only ✗

### The Issue

The user reported that with GDI rendering enabled, text symbols were clustering at the origin. The previous attempts (PR #280, #281, #282) focused on fixing the matrix transformation code in `uzgldrawergdi.pas`, but this code **was never being executed for SHX fonts** because they were using the ZGL rendering path!

The check at `uzgldrawergdi.pas:531`:
```pascal
if not PSymbolsParam^.IsCanSystemDraw then
begin
    inherited;  // Use ZGL rendering (triangles)
    inc(TZGLGDIDrawer(drawer).CurrentPaintGDIData^.DebugCounter.ZGLSymbols);
    exit;
end;
```

This meant that SHX fonts would never reach the GDI system rendering code, regardless of the GDI renderer configuration.

## The Solution

Changed `uzefontshx.pas:39` to enable GDI system rendering for SHX fonts:
```pascal
function TZESHXFontImpl.IsCanSystemDraw:Boolean;
begin
  result:=true;  // Enable GDI system rendering
end;
```

## How It Works After the Fix

Now both font types can use GDI system rendering:
- **TTF fonts**: `IsCanSystemDraw()` returns `true` → Uses GDI rendering ✓
- **SHX fonts**: `IsCanSystemDraw()` returns `true` → Uses GDI rendering ✓

When the GDI renderer is active:
1. Font symbols go through the GDI rendering path (`uzgldrawergdi.pas:515-631`)
2. The system applies proper world transformations for positioning, scaling, rotation, and oblique
3. ExtTextOut renders the symbols at the correct positions using the OS GDI API

## Benefits

1. **Consistent Rendering**: Both TTF and SHX fonts now use the same rendering path
2. **Better Performance**: GDI system rendering can be hardware-accelerated by the OS
3. **Correct Positioning**: Symbols are rendered at their proper positions using the GDI transformation matrix
4. **User Control**: Users can choose between TRT_System, TRT_ZGL, or TRT_Both rendering modes

## Testing Recommendations

1. Load a drawing with SHX fonts
2. Enable GDI rendering mode
3. Verify that text symbols appear at their correct positions (not clustered at origin)
4. Test with various transformations: rotation, scaling, oblique
5. Verify both ASCII and Unicode characters render correctly

## Related Files

- `cad_source/zengine/fonts/uzefontshx.pas` - SHX font implementation
- `cad_source/zengine/zgl/gdi/uzgldrawergdi.pas` - GDI rendering implementation
- `cad_source/zengine/fonts/uzefontfileformatttf.pas` - TTF font implementation (for comparison)

## References

- Issue: https://github.com/veb86/zcadvelecAI/issues/279
- Pull Request: https://github.com/veb86/zcadvelecAI/pull/283
- Previous attempts: PR #280, #281, #282
