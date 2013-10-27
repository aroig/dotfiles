//
//  Copyright (c) 2012-2013 Stefan Bolte <portix@gmx.net>
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//


/*
 * Per domain settings extension
 *
 * This extensions can be used for per-domain-settings. Valid settings are
 * the properties of WebKitWebSettings but in camelcase, see 
 * http://webkitgtk.org/reference/webkitgtk/unstable/WebKitWebSettings.html
 * for details. 
 *
 * To use this extension load it with a userscript in 
 * $HOME/.config/dwb/userscripts/, e.g. 
 *
 * ------------------------------------------------------------------------------
 * |#!javascript                                                                | 
 * |                                                                            | 
 * |extensions.load("perdomainsettings");                                       | 
 * ------------------------------------------------------------------------------
 *
 * The config can consist of four objects, 
 *
 * domains:   Settings applied based on the second level domain
 *
 * hosts:     Settings applied based on the hostname
 *
 * uris:      Settings applied based on the uri
 *
 * defaults:  Default settings, for each setting in domains, hosts and uris a
 *              default-value should be specified
 *
 * Example extensionrc: 
 *
 * ------------------------------------------------------------------------------
 * |return {                                                                    |
 * |   foo : { ... },                                                           |
 * |                                                                            |
 * |   perdomainsettings : {                                                    |
 * |     domains : {                                                            |
 * |        "example.com" : {                                                   |
 * |             "enablePlugins"  : true                                        |
 * |        },                                                                  |
 * |        "example.uk.com" :  {                                               |
 * |             "enablePlugins"  : true,                                       |
 * |             "enableScripts"  : false                                       |
 * |        }                                                                   |
 * |     },                                                                     |
 * |     hosts : {                                                              |
 * |        "www.example1.com" :  {                                             |
 * |             "autoLoadImages" : true                                        |
 * |        }                                                                   |
 * |     },                                                                     |
 * |     uris : {                                                               |
 * |        "http://www.example2.com/login.php" :  {                            |
 * |             "autoLoadImages" : false                                       |
 * |        }                                                                   |
 * |     },                                                                     |
 * |     defaults : {                                                           |
 * |        "enablePlugins"   : false,                                          |
 * |        "autoLoadImages"  : false,                                          |
 * |        "enableScripts"   :  true                                           |
 * |     }                                                                      |
 * |   },                                                                       |
 * |                                                                            |
 * |   bar : { ... }                                                            |
 * |}                                                                           |
 * ------------------------------------------------------------------------------
 *
 * Example using extensions.load: 
 *
 * ------------------------------------------------------------------------------
 * |extensions.load("perdomainsettings", {                                      |
 * |  domains : { "example.com" : { "enablePlugins" : true }  },                |
 * |  defaults : { "enablePlugins" : false }                                    |
 * |});                                                                         |
 * ------------------------------------------------------------------------------
 *
 * */ 
/*<INFO
Change webkit-settings automatically on domain or url basis
INFO>*/

var domains = null;
var hosts = null;
var uris = null;
var exports = {}; 
var defaults = {};


var defaultConfig = {
//<DEFAULT_CONFIG
// Only webkit builtin settings can be set, for a list of settings see 
// http://webkitgtk.org/reference/webkitgtk/unstable/WebKitWebSettings.html
// All settings can also be used in camelcase, otherwise they must be quoted.
// 
// The special domain suffix .tld matches all top level domains, e.g. 
// example.tld matches example.com, example.co.uk, example.it ... 
//
// Settings based on uri will override host based settings and host based
// settings will override domain based settings. Settings for domains/hosts/uris
// with without tld suffix will override settings for
// domains/hosts/uris with tld suffix respectively, e.g. 
//      "example.com" : { enableScripts : true }, 
//      "example.tld" : { enableScripts : false } 
// will enable scripts on example.com but not on example.co.uk, example.it, ... 


// Settings applied based on the second level domain
domains : {
//      "example.com" : { "auto-load-images" : false }, 
//      "google.tld" : { enableScripts : false, autoLoadImages : false }, 
},

//Settings applied based on the hostname
hosts : {
//    "www.example.com" : { autoLoadImages : true } 
},

// Settings applied based on the uri
uris : {
//  "http://www.example.com/foo/" : { autoLoadImages : true } }, 
},

//>DEFAULT_CONFIG
};

function apply(o, settings) 
{
    var key;
    var defaults = true;
    var websettings = o.settings;
    for (key in settings) 
    {
        if (!o.set[key]) 
        {
            websettings[key] = settings[key];
            o.set[key] = true;
        }
        else 
            defaults = false;
    }
    return defaults;
}

function onNavigation(wv, frame, request, action) 
{
    if (wv.mainFrame != frame || !request.message)
        return false;

    var value;

    var o = script.getPrivate(wv, "pds");
    if (! o)
    {
        o = { webview : wv, defaults : false, settings : wv.settings };
        script.setPrivate(wv, "pds", o);
    }
    o.set = {};
    var uri = request.uri;
    var host = request.message.uri.host;
    var domain = net.domainFromHost(host);
    var rTld = new RegExp(domain.substring(domain.indexOf(".") + 1) + "$");
    var domainTld = domain.replace(rTld, "tld");
    var hostTld = host.replace(rTld, "tld");
    var uriTld = uri.replace(new RegExp("(https?://)" + RegExp.escape(host)), "$1" + hostTld);
    if ((value = domains[domain]))
    {
        apply(o, value);
        o.defaults = false;
    }
    if ((value = domains[domainTld]))
    {
        apply(o, value);
        o.defaults = false;
    }
    if ((value = hosts[host])) 
    {
        apply(o, value);
        o.defaults = false;
    }
    if ((value = hosts[hostTld]))
    {
        apply(o, value);
        o.defaults = false;
    }
    if ((value = uris[uri])) 
    {
        apply(o, value);
        o.defaults = false;
    }
    if ((value = uris[uriTld])) 
    {
        apply(o, value);
        o.defaults = false;
    }
    if (o.defaults === false && apply(o, defaults)) 
        o.defaults = true;
    return false;
}
function getDefaultValue(object) 
{
    var key, s, o;
    for(key in object)
    {
        o = object[key];
        for (s in o)
        {
            if (!defaults[s])
                defaults[s] = global.settings[s];
        }
    }
}

var perdomainsettings = {
    exports : exports, 
    init : function (config) 
    {
        if (!config) 
            return false;

        exports.domains = domains = config.domains || defaultConfig.domains;
        domains && getDefaultValue(domains);

        exports.hosts = hosts = config.hosts || defaultConfig.hosts;
        hosts && getDefaultValue(hosts);

        exports.uris = uris = config.uris || defaultConfig.uris;
        uris && getDefaultValue(uris);

        Signal.connect("navigation", onNavigation.debug(script));
        return true;
    },
    end : function () 
    {
        Signal.disconnect(onNavigation);
    }
};
return perdomainsettings;

// vim: set ft=javascript:
