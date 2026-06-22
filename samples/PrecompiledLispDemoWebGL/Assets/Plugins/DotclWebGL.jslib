// Bridges DemoBootstrap.Log -> the page. Appends each line to the output panel
// (#dotcl-out, provided by the WebGL template; created on demand otherwise) and
// mirrors to the browser console.
mergeInto(LibraryManager.library, {
  DotclLog: function (strPtr) {
    var s = UTF8ToString(strPtr);
    var el = document.getElementById('dotcl-out');
    if (!el) {
      el = document.createElement('pre');
      el.id = 'dotcl-out';
      el.style.cssText =
        'position:fixed;left:0;top:0;right:0;max-height:60%;overflow:auto;margin:0;' +
        'padding:8px;background:#111;color:#0f0;font:13px/1.4 monospace;z-index:9999';
      document.body.appendChild(el);
    }
    el.textContent += s + '\n';
    el.scrollTop = el.scrollHeight;   // keep the latest line visible
    if (typeof console !== 'undefined') console.log(s);
  }
});
