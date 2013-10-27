//!javascript
//dwb: Mod1 b

function delicious_bookmark_injected() {
    var e=window
    var n=e.document;
    function blah(){
        function a(e){
            if(e.data==="destroy_bookmarklet"){
                var r=n.getElementById(t);
                if(r){
                    n.body.removeChild(r);
                    r=null
                }
            }
        }

        var t="DELI_bookmarklet_iframe";
        var r=n.getElementById(t);
        if(r) {
            return
        }

        var i="https://delicious.com/save?";
        var s=n.createElement("iframe");
        var url="url="+encodeURIComponent(e.location.href)
        var title="title="+encodeURIComponent(n.title)

        if (e.getSelection) {
            var note="note="+encodeURIComponent(""+e.getSelection())
        } else if (n.getSelection) {
            var note="note="+encodeURIComponent(""+n.getSelection())
        } else {
            var note="note="+encodeURIComponent(""+n.selection.createRange().text)
        }

        s.id=t;
        s.src=i+url+"&"+title+"&"+note+"&v=1.1";
        s.style.position="fixed";
        s.style.top="0";
        s.style.left="0";
        s.style.height="100%";
        s.style.width="100%";
        s.style.zIndex="16777270";
        s.style.border="none";
        s.style.visibility="hidden";
        s.onload=function(){this.style.visibility="visible"};
        n.body.appendChild(s);

        var o=e.addEventListener?"addEventListener":"attachEvent";
        var u=o=="attachEvent"?"onmessage":"message";
        e[o](u,a,false)
    }
    setTimeout(blah,1)
}

/* inject delicious_bookmark function into the web environment */
function delicious_bookmark() {
  tabs.current.inject(delicious_bookmark_injected)
}

bind("Mod1 b", delicious_bookmark, "delicious_bookmark");



