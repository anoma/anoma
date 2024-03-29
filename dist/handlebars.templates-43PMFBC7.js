(()=>{(function(){var d=Handlebars.template,y=Handlebars.templates=Handlebars.templates||{};y["autocomplete-suggestions"]=d({1:function(o,l,a,f,r){var e,n,u=l??(o.nullContext||{}),s=o.hooks.helperMissing,i="function",c=o.escapeExpression,t=o.lookupProperty||function(p,m){if(Object.prototype.hasOwnProperty.call(p,m))return p[m]};return'    <a href="'+c((n=(n=t(a,"link")||(l!=null?t(l,"link"):l))!=null?n:s,typeof n===i?n.call(u,{name:"link",hash:{},data:r,loc:{start:{line:13,column:13},end:{line:13,column:21}}}):n))+'" class="autocomplete-suggestion" data-index="'+c((n=(n=t(a,"index")||r&&t(r,"index"))!=null?n:s,typeof n===i?n.call(u,{name:"index",hash:{},data:r,loc:{start:{line:13,column:67},end:{line:13,column:77}}}):n))+`" tabindex="-1">
      <div class="title">
`+((e=t(a,"if").call(u,l!=null?t(l,"deprecated"):l,{name:"if",hash:{},fn:o.program(2,r,0),inverse:o.program(4,r,0),data:r,loc:{start:{line:15,column:8},end:{line:19,column:15}}}))!=null?e:"")+`
`+((e=t(a,"if").call(u,l!=null?t(l,"label"):l,{name:"if",hash:{},fn:o.program(6,r,0),inverse:o.noop,data:r,loc:{start:{line:21,column:8},end:{line:23,column:15}}}))!=null?e:"")+`      </div>

`+((e=t(a,"if").call(u,l!=null?t(l,"description"):l,{name:"if",hash:{},fn:o.program(8,r,0),inverse:o.noop,data:r,loc:{start:{line:26,column:6},end:{line:30,column:13}}}))!=null?e:"")+`    </a>
`},2:function(o,l,a,f,r){var e,n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return'          <s><span translate="no">'+((e=(n=(n=u(a,"title")||(l!=null?u(l,"title"):l))!=null?n:o.hooks.helperMissing,typeof n=="function"?n.call(l??(o.nullContext||{}),{name:"title",hash:{},data:r,loc:{start:{line:16,column:34},end:{line:16,column:45}}}):n))!=null?e:"")+`</span></s>
`},4:function(o,l,a,f,r){var e,n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return'          <span translate="no">'+((e=(n=(n=u(a,"title")||(l!=null?u(l,"title"):l))!=null?n:o.hooks.helperMissing,typeof n=="function"?n.call(l??(o.nullContext||{}),{name:"title",hash:{},data:r,loc:{start:{line:18,column:31},end:{line:18,column:42}}}):n))!=null?e:"")+`</span>
`},6:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return'          <span class="label">'+o.escapeExpression((e=(e=n(a,"label")||(l!=null?n(l,"label"):l))!=null?e:o.hooks.helperMissing,typeof e=="function"?e.call(l??(o.nullContext||{}),{name:"label",hash:{},data:r,loc:{start:{line:22,column:30},end:{line:22,column:39}}}):e))+`</span>
`},8:function(o,l,a,f,r){var e,n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return`        <div class="description" translate="no">
          `+((e=(n=(n=u(a,"description")||(l!=null?u(l,"description"):l))!=null?n:o.hooks.helperMissing,typeof n=="function"?n.call(l??(o.nullContext||{}),{name:"description",hash:{},data:r,loc:{start:{line:28,column:10},end:{line:28,column:27}}}):n))!=null?e:"")+`
        </div>
`},compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r){var e,n,u=l??(o.nullContext||{}),s=o.lookupProperty||function(i,c){if(Object.prototype.hasOwnProperty.call(i,c))return i[c]};return`<div class="triangle"></div>
<div class="autocomplete-suggestions">
  <div class="autocomplete-results">
    <span class="show">Autocompletion results for 
      <span class="bold"> "`+o.escapeExpression((n=(n=s(a,"term")||(l!=null?s(l,"term"):l))!=null?n:o.hooks.helperMissing,typeof n=="function"?n.call(u,{name:"term",hash:{},data:r,loc:{start:{line:5,column:27},end:{line:5,column:35}}}):n))+`" </span>.
    </span> 
    <span class="mobile-hide">Press 
      <span class="bold">RETURN</span> 
      for full-text search.
    </span>
  </div>
`+((e=s(a,"each").call(u,l!=null?s(l,"suggestions"):l,{name:"each",hash:{},fn:o.program(1,r,0),inverse:o.noop,data:r,loc:{start:{line:12,column:2},end:{line:32,column:11}}}))!=null?e:"")+`</div>
`},useData:!0}),y["modal-layout"]=d({compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r){return`<div id="modal" class="modal" tabindex="-1">
  <div class="modal-contents">
    <div class="modal-header">
      <div class="modal-title"></div>
      <button class="modal-close" aria-label="close">\xD7</button>
    </div>
    <div class="modal-body">
    </div>
  </div>
</div>
`},useData:!0}),y["quick-switch-modal-body"]=d({compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r){return`<div id="quick-switch-modal-body">
  <i class="ri-search-2-line" aria-hidden="true"></i>
  <input type="text" id="quick-switch-input" class="search-input" placeholder="Jump to..." autocomplete="off" spellcheck="false">
  <div id="quick-switch-results"></div>
</div>
`},useData:!0}),y["quick-switch-results"]=d({1:function(o,l,a,f,r){var e,n=l??(o.nullContext||{}),u=o.hooks.helperMissing,s="function",i=o.escapeExpression,c=o.lookupProperty||function(t,p){if(Object.prototype.hasOwnProperty.call(t,p))return t[p]};return'  <div class="quick-switch-result" data-index="'+i((e=(e=c(a,"index")||r&&c(r,"index"))!=null?e:u,typeof e===s?e.call(n,{name:"index",hash:{},data:r,loc:{start:{line:2,column:47},end:{line:2,column:57}}}):e))+`">
    `+i((e=(e=c(a,"name")||(l!=null?c(l,"name"):l))!=null?e:u,typeof e===s?e.call(n,{name:"name",hash:{},data:r,loc:{start:{line:3,column:4},end:{line:3,column:12}}}):e))+`
  </div>
`},compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return(e=n(a,"each").call(l??(o.nullContext||{}),l!=null?n(l,"results"):l,{name:"each",hash:{},fn:o.program(1,r,0),inverse:o.noop,data:r,loc:{start:{line:1,column:0},end:{line:5,column:9}}}))!=null?e:""},useData:!0}),y["search-results"]=d({1:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return"    Search results for <em>"+o.escapeExpression((e=(e=n(a,"value")||(l!=null?n(l,"value"):l))!=null?e:o.hooks.helperMissing,typeof e=="function"?e.call(l??(o.nullContext||{}),{name:"value",hash:{},data:r,loc:{start:{line:3,column:27},end:{line:3,column:36}}}):e))+`</em>
`},3:function(o,l,a,f,r){return`    Invalid search
`},5:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return(e=n(a,"each").call(l??(o.nullContext||{}),l!=null?n(l,"results"):l,{name:"each",hash:{},fn:o.program(6,r,0),inverse:o.noop,data:r,loc:{start:{line:10,column:2},end:{line:21,column:11}}}))!=null?e:""},6:function(o,l,a,f,r){var e,n=o.lambda,u=o.escapeExpression,s=o.lookupProperty||function(i,c){if(Object.prototype.hasOwnProperty.call(i,c))return i[c]};return`    <div class="result">
      <h2 class="result-id">
        <a href="`+u(n(l!=null?s(l,"ref"):l,l))+`">
          <span translate="no">`+u(n(l!=null?s(l,"title"):l,l))+"</span> <small>("+u(n(l!=null?s(l,"type"):l,l))+`)</small>
        </a>
      </h2>
`+((e=s(a,"each").call(l??(o.nullContext||{}),l!=null?s(l,"excerpts"):l,{name:"each",hash:{},fn:o.program(7,r,0),inverse:o.noop,data:r,loc:{start:{line:17,column:8},end:{line:19,column:17}}}))!=null?e:"")+`    </div>
`},7:function(o,l,a,f,r){var e;return'          <p class="result-elem">'+((e=o.lambda(l,l))!=null?e:"")+`</p>
`},9:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return((e=(n(a,"isArray")||l&&n(l,"isArray")||o.hooks.helperMissing).call(l??(o.nullContext||{}),l!=null?n(l,"results"):l,{name:"isArray",hash:{},fn:o.program(10,r,0),inverse:o.program(12,r,0),data:r,loc:{start:{line:23,column:2},end:{line:29,column:14}}}))!=null?e:"")+`
  <p>The search functionality is full-text based. Here are some tips:</p>

  <ul>
    <li>Multiple words (such as <code>foo bar</code>) are searched as <code>OR</code></li>
    <li>Use <code>*</code> anywhere (such as <code>fo*</code>) as wildcard</li>
    <li>Use <code>+</code> before a word (such as <code>+foo</code>) to make its presence required</li>
    <li>Use <code>-</code> before a word (such as <code>-foo</code>) to make its absence required</li>
    <li>Use <code>:</code> to search on a particular field (such as <code>field:word</code>). The available fields are <code>title</code>, <code>doc</code> and <code>type</code></li>
    <li>Use <code>WORD^NUMBER</code> (such as <code>foo^2</code>) to boost the given word</li>
    <li>Use <code>WORD~NUMBER</code> (such as <code>foo~2</code>) to do a search with edit distance on word</li>
  </ul>

  <p>To quickly go to a module, type, or function, use the autocompletion feature in the sidebar search.</p>
`},10:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return"    <p>Sorry, we couldn't find anything for <em>"+o.escapeExpression((e=(e=n(a,"value")||(l!=null?n(l,"value"):l))!=null?e:o.hooks.helperMissing,typeof e=="function"?e.call(l??(o.nullContext||{}),{name:"value",hash:{},data:r,loc:{start:{line:24,column:48},end:{line:24,column:57}}}):e))+`</em>.</p>
`},12:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return(e=n(a,"if").call(l??(o.nullContext||{}),l!=null?n(l,"value"):l,{name:"if",hash:{},fn:o.program(13,r,0),inverse:o.program(15,r,0),data:r,loc:{start:{line:25,column:2},end:{line:29,column:2}}}))!=null?e:""},13:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return"    <p>Invalid search: "+o.escapeExpression((e=(e=n(a,"errorMessage")||(l!=null?n(l,"errorMessage"):l))!=null?e:o.hooks.helperMissing,typeof e=="function"?e.call(l??(o.nullContext||{}),{name:"errorMessage",hash:{},data:r,loc:{start:{line:26,column:23},end:{line:26,column:39}}}):e))+`.</p>
`},15:function(o,l,a,f,r){return`    <p>Please type something into the search bar to perform a search.</p>
  `},compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r){var e,n=l??(o.nullContext||{}),u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return`<h1>
`+((e=u(a,"if").call(n,l!=null?u(l,"value"):l,{name:"if",hash:{},fn:o.program(1,r,0),inverse:o.program(3,r,0),data:r,loc:{start:{line:2,column:2},end:{line:6,column:9}}}))!=null?e:"")+`</h1>

`+((e=(u(a,"isNonEmptyArray")||l&&u(l,"isNonEmptyArray")||o.hooks.helperMissing).call(n,l!=null?u(l,"results"):l,{name:"isNonEmptyArray",hash:{},fn:o.program(5,r,0),inverse:o.program(9,r,0),data:r,loc:{start:{line:9,column:0},end:{line:44,column:20}}}))!=null?e:"")},useData:!0}),y["settings-modal-body"]=d({1:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return(e=n(a,"if").call(l??(o.nullContext||{}),l!=null?n(l,"description"):l,{name:"if",hash:{},fn:o.program(2,r,0),inverse:o.noop,data:r,loc:{start:{line:40,column:6},end:{line:53,column:13}}}))!=null?e:""},2:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return`        <dl class="shortcut-row">
          <dd class="shortcut-description">
            `+o.escapeExpression(o.lambda(l!=null?n(l,"description"):l,l))+`
          </dd>
          <dt class="shortcut-keys">
`+((e=n(a,"if").call(l??(o.nullContext||{}),l!=null?n(l,"displayAs"):l,{name:"if",hash:{},fn:o.program(3,r,0),inverse:o.program(5,r,0),data:r,loc:{start:{line:46,column:12},end:{line:50,column:19}}}))!=null?e:"")+`          </dt>
        </dl>
`},3:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return"              "+((e=o.lambda(l!=null?n(l,"displayAs"):l,l))!=null?e:"")+`
`},5:function(o,l,a,f,r){var e=o.lookupProperty||function(n,u){if(Object.prototype.hasOwnProperty.call(n,u))return n[u]};return"              <kbd><kbd>"+o.escapeExpression(o.lambda(l!=null?e(l,"key"):l,l))+`</kbd></kbd>
`},compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return`<div id="settings-modal-content">
  <div id="settings-content">
    <label class="switch-button-container">
      <div>
        <span>Theme</span>
        <p>Use the documentation UI in a theme.</p>
      </div>
      <div>
        <select name="theme" class="settings-select">
          <option value="dark">Dark</option>
          <option value="light">Light</option>
          <option value="system">System</option>
        </select>
      </div>
    </label>
    <label class="switch-button-container">
      <div>
        <span>Show tooltips</span>
        <p>Show tooltips when mousing over code references.</p>
      </div>
      <div class="switch-button">
        <input class="switch-button__checkbox" type="checkbox" name="tooltips" />
        <div class="switch-button__bg"></div>
      </div>
    </label>
    <label class="switch-button-container">
      <div>
        <span>Run in Livebook</span>
        <p>Use Direct Address for \u201CRun in Livebook\u201D badges.</p>
      </div>
      <div class="switch-button">
        <input class="switch-button__checkbox" type="checkbox" name="direct_livebook_url" />
        <div class="switch-button__bg"></div>
      </div>
    </label>
    <input class="input" type="url" name="livebook_url" placeholder="Enter Livebook instance URL" aria-label="Enter Livebook instance URL" />
  </div>
  <div id="keyboard-shortcuts-content" class="hidden">
`+((e=n(a,"each").call(l??(o.nullContext||{}),l!=null?n(l,"shortcuts"):l,{name:"each",hash:{},fn:o.program(1,r,0),inverse:o.noop,data:r,loc:{start:{line:39,column:4},end:{line:54,column:13}}}))!=null?e:"")+`  </div>
</div>
`},useData:!0}),y["sidebar-items"]=d({1:function(o,l,a,f,r,e,n){var u,s=l??(o.nullContext||{}),i=o.hooks.helperMissing,c=o.lookupProperty||function(t,p){if(Object.prototype.hasOwnProperty.call(t,p))return t[p]};return((u=(c(a,"groupChanged")||l&&c(l,"groupChanged")||i).call(s,n[1],(u=e[0][0])!=null?c(u,"group"):u,{name:"groupChanged",hash:{},fn:o.program(2,r,0,e,n),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:2,column:2},end:{line:6,column:19}}}))!=null?u:"")+`
`+((u=(c(a,"nestingChanged")||l&&c(l,"nestingChanged")||i).call(s,n[1],e[0][0],{name:"nestingChanged",hash:{},fn:o.program(7,r,0,e,n),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:8,column:2},end:{line:10,column:21}}}))!=null?u:"")+`
  <li class="`+((u=(c(a,"isLocal")||l&&c(l,"isLocal")||i).call(s,(u=e[0][0])!=null?c(u,"id"):u,{name:"isLocal",hash:{},fn:o.program(9,r,0,e,n),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:12,column:13},end:{line:12,column:62}}}))!=null?u:"")+`">
    <a href="`+o.escapeExpression(o.lambda((u=e[0][0])!=null?c(u,"id"):u,l))+".html"+((u=(c(a,"isLocal")||l&&c(l,"isLocal")||i).call(s,(u=e[0][0])!=null?c(u,"id"):u,{name:"isLocal",hash:{},fn:o.program(11,r,0,e,n),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:13,column:29},end:{line:13,column:69}}}))!=null?u:"")+'" class="expand" aria-current="'+((u=(c(a,"isLocal")||l&&c(l,"isLocal")||i).call(s,(u=e[0][0])!=null?c(u,"id"):u,{name:"isLocal",hash:{},fn:o.program(13,r,0,e,n),inverse:o.program(15,r,0,e,n),data:r,blockParams:e,loc:{start:{line:13,column:100},end:{line:13,column:149}}}))!=null?u:"")+'" '+((u=(c(a,"isArray")||l&&c(l,"isArray")||i).call(s,(u=e[0][0])!=null?c(u,"headers"):u,{name:"isArray",hash:{},fn:o.program(3,r,0,e,n),inverse:o.program(5,r,0,e,n),data:r,blockParams:e,loc:{start:{line:13,column:151},end:{line:13,column:210}}}))!=null?u:"")+`>
`+((u=c(a,"if").call(s,(u=e[0][0])!=null?c(u,"nested_title"):u,{name:"if",hash:{},fn:o.program(17,r,0,e,n),inverse:o.program(19,r,0,e,n),data:r,blockParams:e,loc:{start:{line:14,column:6},end:{line:18,column:13}}}))!=null?u:"")+`    </a>

`+((u=(c(a,"isEmptyArray")||l&&c(l,"isEmptyArray")||i).call(s,(u=e[0][0])!=null?c(u,"headers"):u,{name:"isEmptyArray",hash:{},fn:o.program(3,r,0,e,n),inverse:o.program(21,r,0,e,n),data:r,blockParams:e,loc:{start:{line:21,column:4},end:{line:24,column:21}}}))!=null?u:"")+`
`+((u=(c(a,"isArray")||l&&c(l,"isArray")||i).call(s,(u=e[0][0])!=null?c(u,"headers"):u,{name:"isArray",hash:{},fn:o.program(24,r,0,e,n),inverse:o.program(28,r,0,e,n),data:r,blockParams:e,loc:{start:{line:26,column:4},end:{line:74,column:16}}}))!=null?u:"")+`  </li>
`},2:function(o,l,a,f,r,e){var n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return'    <li class="group" '+((n=(u(a,"isArray")||l&&u(l,"isArray")||o.hooks.helperMissing).call(l??(o.nullContext||{}),(n=e[1][0])!=null?u(n,"headers"):n,{name:"isArray",hash:{},fn:o.program(3,r,0,e),inverse:o.program(5,r,0,e),data:r,blockParams:e,loc:{start:{line:3,column:22},end:{line:3,column:81}}}))!=null?n:"")+`>
      `+o.escapeExpression(o.lambda((n=e[1][0])!=null?u(n,"group"):n,l))+`
    </li>
`},3:function(o,l,a,f,r){return""},5:function(o,l,a,f,r){return'translate="no"'},7:function(o,l,a,f,r,e){var n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return'    <li class="nesting-context" aria-hidden="true" translate="no">'+o.escapeExpression(o.lambda((n=e[1][0])!=null?u(n,"nested_context"):n,l))+`</li>
`},9:function(o,l,a,f,r){return"current-page open"},11:function(o,l,a,f,r){return"#content"},13:function(o,l,a,f,r){return"page"},15:function(o,l,a,f,r){return"false"},17:function(o,l,a,f,r,e){var n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return"        "+((n=o.lambda((n=e[1][0])!=null?u(n,"nested_title"):n,l))!=null?n:"")+`
`},19:function(o,l,a,f,r,e){var n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return"        "+((n=o.lambda((n=e[1][0])!=null?u(n,"title"):n,l))!=null?n:"")+`
`},21:function(o,l,a,f,r,e){var n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return'      <button class="icon-expand" aria-label="expand" aria-expanded="'+((n=(u(a,"isLocal")||l&&u(l,"isLocal")||o.hooks.helperMissing).call(l??(o.nullContext||{}),(n=e[1][0])!=null?u(n,"id"):n,{name:"isLocal",hash:{},fn:o.program(22,r,0,e),inverse:o.program(15,r,0,e),data:r,blockParams:e,loc:{start:{line:23,column:69},end:{line:23,column:118}}}))!=null?n:"")+'" aria-controls="node-'+o.escapeExpression(o.lambda((n=e[1][0])!=null?u(n,"id"):n,l))+`-headers"></button>
`},22:function(o,l,a,f,r){return"true"},24:function(o,l,a,f,r,e){var n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return(n=(u(a,"isNonEmptyArray")||l&&u(l,"isNonEmptyArray")||o.hooks.helperMissing).call(l??(o.nullContext||{}),(n=e[1][0])!=null?u(n,"headers"):n,{name:"isNonEmptyArray",hash:{},fn:o.program(25,r,0,e),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:27,column:6},end:{line:35,column:26}}}))!=null?n:""},25:function(o,l,a,f,r,e){var n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return'        <ul id="node-'+o.escapeExpression(o.lambda((n=e[2][0])!=null?u(n,"id"):n,l))+`-headers">
`+((n=u(a,"each").call(l??(o.nullContext||{}),(n=e[2][0])!=null?u(n,"headers"):n,{name:"each",hash:{},fn:o.program(26,r,0,e),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:29,column:10},end:{line:33,column:19}}}))!=null?n:"")+`        </ul>
`},26:function(o,l,a,f,r,e){var n,u,s=l??(o.nullContext||{}),i=o.hooks.helperMissing,c="function",t=o.lookupProperty||function(p,m){if(Object.prototype.hasOwnProperty.call(p,m))return p[m]};return`            <li>
              <a href="`+o.escapeExpression(o.lambda((n=e[3][0])!=null?t(n,"id"):n,l))+".html#"+((n=(u=(u=t(a,"anchor")||(l!=null?t(l,"anchor"):l))!=null?u:i,typeof u===c?u.call(s,{name:"anchor",hash:{},data:r,blockParams:e,loc:{start:{line:31,column:40},end:{line:31,column:52}}}):u))!=null?n:"")+'">'+((n=(u=(u=t(a,"id")||(l!=null?t(l,"id"):l))!=null?u:i,typeof u===c?u.call(s,{name:"id",hash:{},data:r,blockParams:e,loc:{start:{line:31,column:54},end:{line:31,column:62}}}):u))!=null?n:"")+`</a>
            </li>
`},28:function(o,l,a,f,r,e){var n,u=l??(o.nullContext||{}),s=o.hooks.helperMissing,i=o.lookupProperty||function(c,t){if(Object.prototype.hasOwnProperty.call(c,t))return c[t]};return'      <ul id="node-'+o.escapeExpression(o.lambda((n=e[1][0])!=null?i(n,"id"):n,l))+`-headers">
`+((n=(i(a,"showSections")||l&&i(l,"showSections")||s).call(u,e[1][0],{name:"showSections",hash:{},fn:o.program(29,r,0,e),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:38,column:8},end:{line:52,column:25}}}))!=null?n:"")+((n=(i(a,"showSummary")||l&&i(l,"showSummary")||s).call(u,e[1][0],{name:"showSummary",hash:{},fn:o.program(34,r,0,e),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:53,column:8},end:{line:57,column:24}}}))!=null?n:"")+((n=i(a,"each").call(u,(n=e[1][0])!=null?i(n,"nodeGroups"):n,{name:"each",hash:{},fn:o.program(36,r,1,e),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:58,column:8},end:{line:72,column:17}}}))!=null?n:"")+`      </ul>
`},29:function(o,l,a,f,r,e){var n,u=l??(o.nullContext||{}),s=o.hooks.helperMissing,i=o.lambda,c=o.escapeExpression,t=o.lookupProperty||function(p,m){if(Object.prototype.hasOwnProperty.call(p,m))return p[m]};return'          <li class="docs '+((n=(t(a,"isLocal")||l&&t(l,"isLocal")||s).call(u,(n=e[2][0])!=null?t(n,"id"):n,{name:"isLocal",hash:{},fn:o.program(30,r,0,e),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:39,column:26},end:{line:39,column:62}}}))!=null?n:"")+`">
            <a href="`+c(i((n=e[2][0])!=null?t(n,"id"):n,l))+`.html#content" class="expand">
              Sections
            </a>
            <button class="icon-expand" aria-label="expand" aria-expanded="`+((n=(t(a,"isLocal")||l&&t(l,"isLocal")||s).call(u,(n=e[2][0])!=null?t(n,"id"):n,{name:"isLocal",hash:{},fn:o.program(22,r,0,e),inverse:o.program(15,r,0,e),data:r,blockParams:e,loc:{start:{line:43,column:75},end:{line:43,column:124}}}))!=null?n:"")+'" aria-controls="'+c(i((n=e[2][0])!=null?t(n,"id"):n,l))+`-sections-list"></button>
            <ul id="`+c(i((n=e[2][0])!=null?t(n,"id"):n,l))+`-sections-list" class="sections-list deflist">
`+((n=t(a,"each").call(u,l!=null?t(l,"sections"):l,{name:"each",hash:{},fn:o.program(32,r,0,e),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:45,column:14},end:{line:49,column:23}}}))!=null?n:"")+`            </ul>
          </li>
`},30:function(o,l,a,f,r){return"open"},32:function(o,l,a,f,r,e){var n,u,s=o.escapeExpression,i=l??(o.nullContext||{}),c=o.hooks.helperMissing,t="function",p=o.lookupProperty||function(m,v){if(Object.prototype.hasOwnProperty.call(m,v))return m[v]};return`                <li>
                  <a href="`+s(o.lambda((n=e[3][0])!=null?p(n,"id"):n,l))+".html#"+s((u=(u=p(a,"anchor")||(l!=null?p(l,"anchor"):l))!=null?u:c,typeof u===t?u.call(i,{name:"anchor",hash:{},data:r,blockParams:e,loc:{start:{line:47,column:44},end:{line:47,column:54}}}):u))+'">'+((n=(u=(u=p(a,"id")||(l!=null?p(l,"id"):l))!=null?u:c,typeof u===t?u.call(i,{name:"id",hash:{},data:r,blockParams:e,loc:{start:{line:47,column:56},end:{line:47,column:64}}}):u))!=null?n:"")+`</a>
                </li>
`},34:function(o,l,a,f,r,e){var n,u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return`          <li>
            <a href="`+o.escapeExpression(o.lambda((n=e[2][0])!=null?u(n,"id"):n,l))+`.html#summary" class="summary">Summary</a>
          </li>
`},36:function(o,l,a,f,r,e){var n,u=o.lambda,s=o.escapeExpression,i=o.lookupProperty||function(c,t){if(Object.prototype.hasOwnProperty.call(c,t))return c[t]};return`          <li class="docs">
            <a href="`+s(u((n=e[2][0])!=null?i(n,"id"):n,l))+".html#"+s(u((n=e[0][0])!=null?i(n,"key"):n,l))+`" class="expand">
              `+s(u((n=e[0][0])!=null?i(n,"name"):n,l))+`
            </a>
            <button class="icon-expand" aria-label="expand" aria-expanded="false" aria-controls="node-`+s(u((n=e[2][0])!=null?i(n,"id"):n,l))+"-group-"+s(u((n=e[0][0])!=null?i(n,"key"):n,l))+`-list"></button>
            <ul id="node-`+s(u((n=e[2][0])!=null?i(n,"id"):n,l))+"-group-"+s(u((n=e[0][0])!=null?i(n,"key"):n,l))+'-list" class="'+s(u((n=e[0][0])!=null?i(n,"key"):n,l))+`-list deflist">
`+((n=i(a,"each").call(l??(o.nullContext||{}),(n=e[0][0])!=null?i(n,"nodes"):n,{name:"each",hash:{},fn:o.program(37,r,0,e),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:65,column:14},end:{line:69,column:23}}}))!=null?n:"")+`            </ul>
          </li>
`},37:function(o,l,a,f,r,e){var n,u,s=o.escapeExpression,i=l??(o.nullContext||{}),c=o.hooks.helperMissing,t="function",p=o.lookupProperty||function(m,v){if(Object.prototype.hasOwnProperty.call(m,v))return m[v]};return`                <li>
                  <a href="`+s(o.lambda((n=e[3][0])!=null?p(n,"id"):n,l))+".html#"+s((u=(u=p(a,"anchor")||(l!=null?p(l,"anchor"):l))!=null?u:c,typeof u===t?u.call(i,{name:"anchor",hash:{},data:r,blockParams:e,loc:{start:{line:67,column:44},end:{line:67,column:54}}}):u))+'" title="'+s((u=(u=p(a,"title")||(l!=null?p(l,"title"):l))!=null?u:c,typeof u===t?u.call(i,{name:"title",hash:{},data:r,blockParams:e,loc:{start:{line:67,column:63},end:{line:67,column:72}}}):u))+'" translate="no">'+s((u=(u=p(a,"id")||(l!=null?p(l,"id"):l))!=null?u:c,typeof u===t?u.call(i,{name:"id",hash:{},data:r,blockParams:e,loc:{start:{line:67,column:89},end:{line:67,column:95}}}):u))+`</a>
                </li>
`},compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r,e,n){var u,s=o.lookupProperty||function(i,c){if(Object.prototype.hasOwnProperty.call(i,c))return i[c]};return(u=s(a,"each").call(l??(o.nullContext||{}),l!=null?s(l,"nodes"):l,{name:"each",hash:{},fn:o.program(1,r,2,e,n),inverse:o.noop,data:r,blockParams:e,loc:{start:{line:1,column:0},end:{line:76,column:9}}}))!=null?u:""},useData:!0,useDepths:!0,useBlockParams:!0}),y.tabset=d({1:function(o,l,a,f,r){var e,n,u=l??(o.nullContext||{}),s=o.hooks.helperMissing,i="function",c=o.escapeExpression,t=o.lookupProperty||function(p,m){if(Object.prototype.hasOwnProperty.call(p,m))return p[m]};return'    <button role="tab" id="tab-'+c((n=(n=t(a,"setIndex")||(l!=null?t(l,"setIndex"):l))!=null?n:s,typeof n===i?n.call(u,{name:"setIndex",hash:{},data:r,loc:{start:{line:3,column:31},end:{line:3,column:43}}}):n))+"-"+c((n=(n=t(a,"index")||r&&t(r,"index"))!=null?n:s,typeof n===i?n.call(u,{name:"index",hash:{},data:r,loc:{start:{line:3,column:44},end:{line:3,column:54}}}):n))+`" class="tabset-tab"
    tabindex="`+((e=t(a,"if").call(u,r&&t(r,"index"),{name:"if",hash:{},fn:o.program(2,r,0),inverse:o.program(4,r,0),data:r,loc:{start:{line:4,column:14},end:{line:4,column:46}}}))!=null?e:"")+`"
    aria-selected="`+((e=t(a,"if").call(u,r&&t(r,"index"),{name:"if",hash:{},fn:o.program(6,r,0),inverse:o.program(8,r,0),data:r,loc:{start:{line:5,column:19},end:{line:5,column:57}}}))!=null?e:"")+`"
    aria-controls="tabpanel-`+c((n=(n=t(a,"setIndex")||(l!=null?t(l,"setIndex"):l))!=null?n:s,typeof n===i?n.call(u,{name:"setIndex",hash:{},data:r,loc:{start:{line:6,column:28},end:{line:6,column:40}}}):n))+"-"+c((n=(n=t(a,"index")||r&&t(r,"index"))!=null?n:s,typeof n===i?n.call(u,{name:"index",hash:{},data:r,loc:{start:{line:6,column:41},end:{line:6,column:51}}}):n))+`">
      `+c(o.lambda(l!=null?t(l,"label"):l,l))+`
    </button>
`},2:function(o,l,a,f,r){return"-1"},4:function(o,l,a,f,r){return"0"},6:function(o,l,a,f,r){return"false"},8:function(o,l,a,f,r){return"true"},10:function(o,l,a,f,r){var e,n,u=l??(o.nullContext||{}),s=o.hooks.helperMissing,i="function",c=o.escapeExpression,t=o.lookupProperty||function(p,m){if(Object.prototype.hasOwnProperty.call(p,m))return p[m]};return'  <div role="tabpanel" id="tabpanel-'+c((n=(n=t(a,"setIndex")||(l!=null?t(l,"setIndex"):l))!=null?n:s,typeof n===i?n.call(u,{name:"setIndex",hash:{},data:r,loc:{start:{line:13,column:36},end:{line:13,column:48}}}):n))+"-"+c((n=(n=t(a,"index")||r&&t(r,"index"))!=null?n:s,typeof n===i?n.call(u,{name:"index",hash:{},data:r,loc:{start:{line:13,column:49},end:{line:13,column:59}}}):n))+`" class="tabset-panel"
    `+((e=t(a,"if").call(u,r&&t(r,"index"),{name:"if",hash:{},fn:o.program(11,r,0),inverse:o.noop,data:r,loc:{start:{line:14,column:4},end:{line:14,column:31}}}))!=null?e:"")+' tabindex="'+((e=t(a,"if").call(u,r&&t(r,"index"),{name:"if",hash:{},fn:o.program(2,r,0),inverse:o.program(4,r,0),data:r,loc:{start:{line:14,column:42},end:{line:14,column:74}}}))!=null?e:"")+`"
    aria-labelledby="tab-`+c((n=(n=t(a,"setIndex")||(l!=null?t(l,"setIndex"):l))!=null?n:s,typeof n===i?n.call(u,{name:"setIndex",hash:{},data:r,loc:{start:{line:15,column:25},end:{line:15,column:37}}}):n))+"-"+c((n=(n=t(a,"index")||r&&t(r,"index"))!=null?n:s,typeof n===i?n.call(u,{name:"index",hash:{},data:r,loc:{start:{line:15,column:38},end:{line:15,column:48}}}):n))+`">
`+((e=t(a,"each").call(u,l!=null?t(l,"content"):l,{name:"each",hash:{},fn:o.program(13,r,0),inverse:o.noop,data:r,loc:{start:{line:16,column:4},end:{line:18,column:13}}}))!=null?e:"")+`  </div>
`},11:function(o,l,a,f,r){return"hidden"},13:function(o,l,a,f,r){var e;return"      "+((e=o.lambda(l,l))!=null?e:"")+`
`},compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r){var e,n=l??(o.nullContext||{}),u=o.lookupProperty||function(s,i){if(Object.prototype.hasOwnProperty.call(s,i))return s[i]};return`<div role="tablist" class="tabset-tablist">
`+((e=u(a,"each").call(n,l!=null?u(l,"tabs"):l,{name:"each",hash:{},fn:o.program(1,r,0),inverse:o.noop,data:r,loc:{start:{line:2,column:2},end:{line:9,column:11}}}))!=null?e:"")+`</div>

`+((e=u(a,"each").call(n,l!=null?u(l,"tabs"):l,{name:"each",hash:{},fn:o.program(10,r,0),inverse:o.noop,data:r,loc:{start:{line:12,column:0},end:{line:20,column:9}}}))!=null?e:"")},useData:!0}),y["tooltip-body"]=d({1:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return`  <section class="docstring docstring-plain">
    `+o.escapeExpression(o.lambda((e=l!=null?n(l,"hint"):l)!=null?n(e,"description"):e,l))+`
  </section>
`},3:function(o,l,a,f,r){var e,n=o.lambda,u=o.escapeExpression,s=o.lookupProperty||function(i,c){if(Object.prototype.hasOwnProperty.call(i,c))return i[c]};return`  <div class="detail-header">
    <h1 class="signature">
      <span translate="no">`+u(n((e=l!=null?s(l,"hint"):l)!=null?s(e,"title"):e,l))+`</span>
      <div class="version-info" translate="no">`+u(n((e=l!=null?s(l,"hint"):l)!=null?s(e,"version"):e,l))+`</div>
    </h1>
  </div>
`+((e=s(a,"if").call(l??(o.nullContext||{}),(e=l!=null?s(l,"hint"):l)!=null?s(e,"description"):e,{name:"if",hash:{},fn:o.program(4,r,0),inverse:o.noop,data:r,loc:{start:{line:12,column:2},end:{line:16,column:9}}}))!=null?e:"")},4:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return`    <section class="docstring">
      `+((e=o.lambda((e=l!=null?n(l,"hint"):l)!=null?n(e,"description"):e,l))!=null?e:"")+`
    </section>
`},compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return(e=n(a,"if").call(l??(o.nullContext||{}),l!=null?n(l,"isPlain"):l,{name:"if",hash:{},fn:o.program(1,r,0),inverse:o.program(3,r,0),data:r,loc:{start:{line:1,column:0},end:{line:17,column:7}}}))!=null?e:""},useData:!0}),y["tooltip-layout"]=d({compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r){return`<div id="tooltip">
  <div class="tooltip-body"></div>
</div>
`},useData:!0}),y["versions-dropdown"]=d({1:function(o,l,a,f,r){var e,n,u=l??(o.nullContext||{}),s=o.hooks.helperMissing,i="function",c=o.escapeExpression,t=o.lookupProperty||function(p,m){if(Object.prototype.hasOwnProperty.call(p,m))return p[m]};return'        <option translate="no" value="'+c((n=(n=t(a,"url")||(l!=null?t(l,"url"):l))!=null?n:s,typeof n===i?n.call(u,{name:"url",hash:{},data:r,loc:{start:{line:7,column:38},end:{line:7,column:45}}}):n))+'"'+((e=t(a,"if").call(u,l!=null?t(l,"isCurrentVersion"):l,{name:"if",hash:{},fn:o.program(2,r,0),inverse:o.noop,data:r,loc:{start:{line:7,column:46},end:{line:7,column:95}}}))!=null?e:"")+`>
          `+c((n=(n=t(a,"version")||(l!=null?t(l,"version"):l))!=null?n:s,typeof n===i?n.call(u,{name:"version",hash:{},data:r,loc:{start:{line:8,column:10},end:{line:8,column:21}}}):n))+`
        </option>
`},2:function(o,l,a,f,r){return" selected disabled"},compiler:[8,">= 4.3.0"],main:function(o,l,a,f,r){var e,n=o.lookupProperty||function(u,s){if(Object.prototype.hasOwnProperty.call(u,s))return u[s]};return`<form autocomplete="off">
  <label>
    <span class="sidebar-projectVersionsDropdownCaret" aria-hidden="true">&#x25bc;</span>
    <span class="sr-only">Project version</span>
    <select class="sidebar-projectVersionsDropdown">
`+((e=n(a,"each").call(l??(o.nullContext||{}),l!=null?n(l,"nodes"):l,{name:"each",hash:{},fn:o.program(1,r,0),inverse:o.noop,data:r,loc:{start:{line:6,column:6},end:{line:10,column:15}}}))!=null?e:"")+`    </select>
  </label>
</form>
`},useData:!0})})();})();
