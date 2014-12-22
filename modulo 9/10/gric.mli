type form =
    True
  | False
  | Prop of string
  | Not of form
  | And of form * form
  | Or of form * form
  | Imp of form * form
val fnn : form -> form
val dnf : form -> form
val mkand : form list -> form
val mkdnf : form list list -> form
val complement : form -> form
val alpha : form -> form * form
val beta : form -> form * form
val all_models : form list -> form list list
val dnf_tab : form -> form
