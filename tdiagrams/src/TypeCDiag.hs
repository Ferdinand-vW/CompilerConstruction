import CCO.Diag       (Diag,checkTy)
import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Tree       (ATerm, Tree (toTree,fromTree), parser)
import Control.Arrow  (Arrow(arr),(>>>))

main :: IO ()
main = ioWrap (parser >>> (component toTree :: Component ATerm Diag) >>> component checkTy >>> arr fromTree >>> printer)  