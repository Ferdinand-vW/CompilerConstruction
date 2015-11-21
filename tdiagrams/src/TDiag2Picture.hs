import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Picture.AG (Diag (Diag))
import CCO.Picture.Base
import CCO.Picture    (Picture)
import CCO.Tree       (ATerm, Tree (toTree,fromTree), parser)
import Control.Arrow  (Arrow (arr),(>>>))

main = ioWrap (parser >>> (component toTree :: Component ATerm Diag) >>> component eval >>> arr fromTree >>> printer)