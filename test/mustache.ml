open Tyxml

let%template f ={|
Hello {{name}}
You have just won {{value}} dollars!
{{#repo}}
  <b>{{foo}}</b> {{name}}
{{/repo}}
{{^repo}}
  No repos :(
{{/repo}}
|}
