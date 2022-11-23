# RealtimeSharableFieldMacro
 Realtime Sharable Field Macro for haxe

# How to Use

## Create Sharable Field
 Add meta '@field' front of just variables (with out public or private keyword, also static).

## About get/set Callback
 Create a function whose name is 'getCallback' or 'setCallback'. The macro will then use that function to automatically return or save values.