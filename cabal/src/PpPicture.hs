import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Picture    (Picture)
import CCO.Tree       (ATerm, Tree (toTree), parser)
import Control.Arrow  ((>>>))

main =
  ioWrap (parser >>> (component toTree :: Component ATerm Picture) >>> printer)