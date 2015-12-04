import CCO.Component    (printer, ioWrap)
import CCO.HM           (parser)
import CCO.Tree         (fromTree)
import Control.Arrow    (arr, (>>>))

main = ioWrap (parser >>> (component toTree :: Component ATerm Tm) >>> component hm2cr >>> arr fromTree >>> printer)