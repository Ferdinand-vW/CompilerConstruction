import CCO.Component    (Component, component, printer, ioWrap)
import CCO.HM           (parser, toANormal)
import CCO.Tree         (fromTree)
import Control.Arrow    (arr, (>>>))

--main = ioWrap (parser >>>  arr fromTree >>> printer)
main = ioWrap (parser >>> arr fromTree >>> printer)