//!javascript
//<formfiller___SCRIPT
extensions.load("formfiller", {
//<formfiller___CONFIG
// shortcut that gets and saves formdata
scGetForm : "efg",

// shortcut that fills a form
scFillForm : "eff",

// path to the formdata file
formData : data.configDir + "/forms",

// whether to use a gpg-encrypted file
useGPG : false,

// your GPG key ID (leave empty to use a symmetric cipher)
GPGKeyID : "",

// whether to use a GPG agent (requires non-empty GPGKeyID to work)
GPGAgent : false,

// additional arguments passed to gpg2 when encrypting the formdata
GPGOptEncrypt : "",

// additional arguments passed to gpg2 when decrypting the formdata
GPGOptDecrypt : "",

// whether to save the password in memory when gpg is used
keepPassword : true,

// whether to save the whole formdata in memory when gpg is used
keepFormdata : false

//>formfiller___CONFIG
});
//>formfiller___SCRIPT
