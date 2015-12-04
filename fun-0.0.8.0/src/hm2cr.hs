import CCO.Component    (Component,component,printer, ioWrap)
import CCO.Core.AG (Tm)
import CCO.Core         (hm2cr)
import CCO.Tree         (parser,ATerm,fromTree,toTree)
import Control.Arrow    (arr, (>>>))

main = ioWrap (parser >>> (component toTree :: Component ATerm Tm) >>> component hm2cr >>> arr fromTree >>> printer)