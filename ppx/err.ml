let pattern loc =
  Location.raise_errorf ~loc "Expecting a singleton variable pattern!"
let payload loc =
  Location.raise_errorf ~loc "Expecting a single value or binding!"
let const_exp loc = 
  Location.raise_errorf ~loc "Expecting a single constant expression!"
