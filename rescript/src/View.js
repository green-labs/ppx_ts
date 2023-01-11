// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Spice = require("@greenlabs/ppx-spice/src/rescript/Spice.js");
var Js_dict = require("rescript/lib/js/js_dict.js");
var Js_json = require("rescript/lib/js/js_json.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

function err_encode(v) {
  return Js_dict.fromArray([
              [
                "firstName",
                Spice.stringToJson(v.firstName)
              ],
              [
                "lastName",
                Spice.stringToJson(v.lastName)
              ]
            ]);
}

function err_decode(v) {
  var dict = Js_json.classify(v);
  if (typeof dict === "number") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (dict.TAG !== /* JSONObject */2) {
    return Spice.error(undefined, "Not an object", v);
  }
  var dict$1 = dict._0;
  var firstName = Spice.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(dict$1, "firstName"), null));
  if (firstName.TAG === /* Ok */0) {
    var lastName = Spice.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(dict$1, "lastName"), null));
    if (lastName.TAG === /* Ok */0) {
      return {
              TAG: /* Ok */0,
              _0: {
                firstName: firstName._0,
                lastName: lastName._0
              }
            };
    }
    var e = lastName._0;
    return {
            TAG: /* Error */1,
            _0: {
              path: ".lastName" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = firstName._0;
  return {
          TAG: /* Error */1,
          _0: {
            path: ".firstName" + e$1.path,
            message: e$1.message,
            value: e$1.value
          }
        };
}

var Err = {
  err_encode: err_encode,
  err_decode: err_decode
};

function t_keyToString(key) {
  switch (key) {
    case /* Name */0 :
        return "name";
    case /* Age */1 :
        return "age";
    case /* IsKorean */2 :
        return "isKorean";
    
  }
}

function t1_encode(v) {
  if (v) {
    return ["LastName"];
  } else {
    return ["FirstName"];
  }
}

function t1_decode(v) {
  var json_arr = Js_json.classify(v);
  if (typeof json_arr === "number") {
    return Spice.error(undefined, "Not a variant", v);
  }
  if (json_arr.TAG !== /* JSONArray */3) {
    return Spice.error(undefined, "Not a variant", v);
  }
  var json_arr$1 = json_arr._0;
  if (json_arr$1.length === 0) {
    return Spice.error(undefined, "Expected variant, found empty array", v);
  }
  var tagged = json_arr$1.map(Js_json.classify);
  var match = Belt_Array.getExn(tagged, 0);
  if (typeof match !== "number" && match.TAG === /* JSONString */0) {
    switch (match._0) {
      case "FirstName" :
          if (tagged.length !== 1) {
            return Spice.error(undefined, "Invalid number of arguments to variant constructor", v);
          } else {
            return {
                    TAG: /* Ok */0,
                    _0: /* FirstName */0
                  };
          }
      case "LastName" :
          if (tagged.length !== 1) {
            return Spice.error(undefined, "Invalid number of arguments to variant constructor", v);
          } else {
            return {
                    TAG: /* Ok */0,
                    _0: /* LastName */1
                  };
          }
      default:
        
    }
  }
  return Spice.error(undefined, "Invalid variant constructor", Belt_Array.getExn(json_arr$1, 0));
}

function t1_keyToString(key) {
  if (key) {
    return "lastName";
  } else {
    return "firstName";
  }
}

function t2_encode(v) {
  switch (v) {
    case /* Name */0 :
        return ["Name"];
    case /* Age */1 :
        return ["Age"];
    case /* IsKorean */2 :
        return ["IsKorean"];
    
  }
}

function t2_decode(v) {
  var json_arr = Js_json.classify(v);
  if (typeof json_arr === "number") {
    return Spice.error(undefined, "Not a variant", v);
  }
  if (json_arr.TAG !== /* JSONArray */3) {
    return Spice.error(undefined, "Not a variant", v);
  }
  var json_arr$1 = json_arr._0;
  if (json_arr$1.length === 0) {
    return Spice.error(undefined, "Expected variant, found empty array", v);
  }
  var tagged = json_arr$1.map(Js_json.classify);
  var match = Belt_Array.getExn(tagged, 0);
  if (typeof match !== "number" && match.TAG === /* JSONString */0) {
    switch (match._0) {
      case "Age" :
          if (tagged.length !== 1) {
            return Spice.error(undefined, "Invalid number of arguments to variant constructor", v);
          } else {
            return {
                    TAG: /* Ok */0,
                    _0: /* Age */1
                  };
          }
      case "IsKorean" :
          if (tagged.length !== 1) {
            return Spice.error(undefined, "Invalid number of arguments to variant constructor", v);
          } else {
            return {
                    TAG: /* Ok */0,
                    _0: /* IsKorean */2
                  };
          }
      case "Name" :
          if (tagged.length !== 1) {
            return Spice.error(undefined, "Invalid number of arguments to variant constructor", v);
          } else {
            return {
                    TAG: /* Ok */0,
                    _0: /* Name */0
                  };
          }
      default:
        
    }
  }
  return Spice.error(undefined, "Invalid variant constructor", Belt_Array.getExn(json_arr$1, 0));
}

function t2_keyToString(key) {
  switch (key) {
    case /* Name */0 :
        return "name";
    case /* Age */1 :
        return "age";
    case /* IsKorean */2 :
        return "isKorean";
    
  }
}

function t3_encode(encoder_a, v) {
  return Js_dict.fromArray([
              [
                "firstName",
                Curry._1(encoder_a, v.firstName)
              ],
              [
                "lastName",
                Curry._1(encoder_a, v.lastName)
              ]
            ]);
}

function t3_decode(decoder_a, v) {
  var dict = Js_json.classify(v);
  if (typeof dict === "number") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (dict.TAG !== /* JSONObject */2) {
    return Spice.error(undefined, "Not an object", v);
  }
  var dict$1 = dict._0;
  var firstName = Curry._1(decoder_a, Belt_Option.getWithDefault(Js_dict.get(dict$1, "firstName"), null));
  if (firstName.TAG === /* Ok */0) {
    var lastName = Curry._1(decoder_a, Belt_Option.getWithDefault(Js_dict.get(dict$1, "lastName"), null));
    if (lastName.TAG === /* Ok */0) {
      return {
              TAG: /* Ok */0,
              _0: {
                firstName: firstName._0,
                lastName: lastName._0
              }
            };
    }
    var e = lastName._0;
    return {
            TAG: /* Error */1,
            _0: {
              path: ".lastName" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = firstName._0;
  return {
          TAG: /* Error */1,
          _0: {
            path: ".firstName" + e$1.path,
            message: e$1.message,
            value: e$1.value
          }
        };
}

function t4_encode(encoder_a, v) {
  return Js_dict.fromArray([
              [
                "name",
                Curry._1(encoder_a, v.name)
              ],
              [
                "age",
                Curry._1(encoder_a, v.age)
              ],
              [
                "isKorean",
                Curry._1(encoder_a, v.isKorean)
              ]
            ]);
}

function t4_decode(decoder_a, v) {
  var dict = Js_json.classify(v);
  if (typeof dict === "number") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (dict.TAG !== /* JSONObject */2) {
    return Spice.error(undefined, "Not an object", v);
  }
  var dict$1 = dict._0;
  var name = Curry._1(decoder_a, Belt_Option.getWithDefault(Js_dict.get(dict$1, "name"), null));
  if (name.TAG === /* Ok */0) {
    var age = Curry._1(decoder_a, Belt_Option.getWithDefault(Js_dict.get(dict$1, "age"), null));
    if (age.TAG === /* Ok */0) {
      var isKorean = Curry._1(decoder_a, Belt_Option.getWithDefault(Js_dict.get(dict$1, "isKorean"), null));
      if (isKorean.TAG === /* Ok */0) {
        return {
                TAG: /* Ok */0,
                _0: {
                  name: name._0,
                  age: age._0,
                  isKorean: isKorean._0
                }
              };
      }
      var e = isKorean._0;
      return {
              TAG: /* Error */1,
              _0: {
                path: ".isKorean" + e.path,
                message: e.message,
                value: e.value
              }
            };
    }
    var e$1 = age._0;
    return {
            TAG: /* Error */1,
            _0: {
              path: ".age" + e$1.path,
              message: e$1.message,
              value: e$1.value
            }
          };
  }
  var e$2 = name._0;
  return {
          TAG: /* Error */1,
          _0: {
            path: ".name" + e$2.path,
            message: e$2.message,
            value: e$2.value
          }
        };
}

function t5_encode(v) {
  return Js_dict.fromArray([
              [
                "name",
                err_encode(v.name)
              ],
              [
                "age",
                err_encode(v.age)
              ],
              [
                "isKorean",
                err_encode(v.isKorean)
              ]
            ]);
}

function t5_decode(v) {
  var dict = Js_json.classify(v);
  if (typeof dict === "number") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (dict.TAG !== /* JSONObject */2) {
    return Spice.error(undefined, "Not an object", v);
  }
  var dict$1 = dict._0;
  var name = err_decode(Belt_Option.getWithDefault(Js_dict.get(dict$1, "name"), null));
  if (name.TAG === /* Ok */0) {
    var age = err_decode(Belt_Option.getWithDefault(Js_dict.get(dict$1, "age"), null));
    if (age.TAG === /* Ok */0) {
      var isKorean = err_decode(Belt_Option.getWithDefault(Js_dict.get(dict$1, "isKorean"), null));
      if (isKorean.TAG === /* Ok */0) {
        return {
                TAG: /* Ok */0,
                _0: {
                  name: name._0,
                  age: age._0,
                  isKorean: isKorean._0
                }
              };
      }
      var e = isKorean._0;
      return {
              TAG: /* Error */1,
              _0: {
                path: ".isKorean" + e.path,
                message: e.message,
                value: e.value
              }
            };
    }
    var e$1 = age._0;
    return {
            TAG: /* Error */1,
            _0: {
              path: ".age" + e$1.path,
              message: e$1.message,
              value: e$1.value
            }
          };
  }
  var e$2 = name._0;
  return {
          TAG: /* Error */1,
          _0: {
            path: ".name" + e$2.path,
            message: e$2.message,
            value: e$2.value
          }
        };
}

var t8_toArray = [
  "Name",
  "Age"
];

var t9_toArray = [
  "Name",
  "Age"
];

exports.Err = Err;
exports.t_keyToString = t_keyToString;
exports.t1_encode = t1_encode;
exports.t1_decode = t1_decode;
exports.t1_keyToString = t1_keyToString;
exports.t2_encode = t2_encode;
exports.t2_decode = t2_decode;
exports.t2_keyToString = t2_keyToString;
exports.t3_encode = t3_encode;
exports.t3_decode = t3_decode;
exports.t4_encode = t4_encode;
exports.t4_decode = t4_decode;
exports.t5_encode = t5_encode;
exports.t5_decode = t5_decode;
exports.t8_toArray = t8_toArray;
exports.t9_toArray = t9_toArray;
/* No side effect */
