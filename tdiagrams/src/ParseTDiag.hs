import CCO.Component  (printer, ioWrap)
import CCO.Diag       (parser)
import CCO.Tree       (ATerm(App),Tree (fromTree))
import Control.Arrow  (Arrow (arr), (>>>))

main = ioWrap (parser >>> arr fromTree >>> printer)