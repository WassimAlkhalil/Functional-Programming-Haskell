> module My where 
> import Numeric hiding (floattoDigits)
> import Numeric (showSigned,fromRat,showFloat)
  
The module name is capitalized. Note the keyword where.
Convention: one module per file
        file name = module name with suffix .hs or .lhs.
All definitions in the module must begin in the same column as the keyword module.


A module can import other modules to use their definitions. The import statement starts in the same column as the keyword module.
After the import, all visible definitions of the imported module can be used. By default, all its top-level definitions are visible.
