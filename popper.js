(()=>{var so=function(n,r){if(n<1)return[];var t=new Array(n);return t.fill(r)},_o=function(n,r){for(var t=[],o=0,e=0;e<n;e++)t[o++]=r;return t},Do=typeof Array.prototype.fill=="function"?so:_o;var Et=function(n){return function(r){for(var t=r.length,o=new Array(t),e=0;e<t;e++)o[e]=n(r[e]);return o}};var Fr={compose:function(n){return function(r){return function(t){return n(r(t))}}}};var R=function(n){return n.identity},N={identity:function(n){return n},Semigroupoid0:function(){return Fr}};var gn=function(n){return function(r){return function(t){return n(t)(r)}}},V=function(n){return function(r){return n}};var c=function(n){return n.map};var $=function(n){return c(n)(V(void 0))};var on={map:Et};var d=function(n){return n.append};var Eo=R(N);var U=function(n){return n.apply};var Er=function(n){var r=U(n),t=c(n.Functor0());return function(o){return function(e){return r(t(V(Eo))(o))(e)}}};var F=function(n){return n.pure};var jn=function(n){var r=U(n.Apply0()),t=F(n);return function(o){return function(e){return r(t(o))(e)}}};var B=function(n){return n.bind};var Tr=function(n){var r=B(n);return function(t){return function(o){return function(e){return r(t)(function(a){return a?o:e})}}}};var Jn=function(n){var r=B(n.Bind1()),t=F(n.Applicative0());return function(o){return function(e){return r(o)(function(a){return r(e)(function(i){return t(a(i))})})}}};var Ao=Number.POSITIVE_INFINITY,Po=Number.NEGATIVE_INFINITY;var Ro=function(n){return function(r){return function(t){return function(o){return function(e){return o<e?n:o===e?r:t}}}}};var At=Ro;var $o=function(n){return function(r){return n===r}};var Pt=$o;var qt={eq:Pt};var P=function(){function n(){}return n.value=new n,n}(),q=function(){function n(){}return n.value=new n,n}(),H=function(){function n(){}return n.value=new n,n}();var Wn=function(){return{compare:At(P.value)(H.value)(q.value),Eq0:function(){return qt}}}();var s=function(n){return n.compare};var Ar=function(n){var r=s(n);return function(t){return function(o){var e=r(t)(o);if(e instanceof P)return o;if(e instanceof H||e instanceof q)return t;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[e.constructor.name])}}},Pr=function(n){var r=s(n);return function(t){return function(o){var e=r(t)(o);if(e instanceof P||e instanceof H)return t;if(e instanceof q)return o;throw new Error("Failed pattern match at Data.Ord (line 172, column 3 - line 175, column 12): "+[e.constructor.name])}}};var Rt=function(n){var r=n.toString();return isNaN(r+".0")?r:r+".0"};var It={show:Rt};var m=function(n){return n.show};var D=function(){function n(){}return n.value=new n,n}(),y=function(){function n(r){this.value0=r}return n.create=function(r){return new n(r)},n}();var J=function(n){return n.mempty};var Ut=function(n){return function(){return n}},kt=function(n){return function(r){return function(){return r(n())()}}};var Lt=function(n,r,t){var o=0,e;return function(a){if(o===2)return e;if(o===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return o=1,e=t(),o=2,e}},Nt={Applicative0:function(){return en},Bind1:function(){return Qn}},Qn={bind:kt,Apply0:function(){return jt(0)}},en={pure:Ut,Apply0:function(){return jt(0)}},Bt=Lt("functorEffect","Effect",function(){return{map:jn(en)}}),jt=Lt("applyEffect","Effect",function(){return{apply:Jn(Nt),Functor0:function(){return Bt(0)}}}),Dn=Bt(20);var Gt=function(n){return function(r){return function(){return n(r())}}},Qt=function(n){return function(){return n}},Vt=function(n){return function(r){return function(){return r(n())()}}};var ia=function(n,r,t){var o=0,e;return function(a){if(o===2)return e;if(o===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+r+", line "+a+")",r,a);return o=1,e=t(),o=2,e}};var Rn={map:Gt};var $r={Applicative0:function(){return Xt},Bind1:function(){return Vn}},Vn={bind:Vt,Apply0:function(){return Zt(0)}},Xt={pure:Qt,Apply0:function(){return Zt(0)}},Zt=ia("applyST","Control.Monad.ST.Internal",function(){return{apply:Jn($r),Functor0:function(){return Rn}}});function ca(n){return n}var ne=ca;function fa(n){return n.slice()}var re=fa;var te=function(n,r){return r.push(n)};var Yn=function(r){return function(t){return function(){return r(t)}}},Or=function(r){return function(t){return function(o){return function(){return r(t,o)}}}};var ee=Yn(ne);var da=Yn(re),oe=function(n){return function(r){return function(){var o=da(r)();return n(o)(),ee(o)()}}};var Ur=Or(te);var ce=function(n){return function(r){return function(t){for(var o=r,e=t.length,a=e-1;a>=0;a--)o=n(t[a])(o);return o}}},fe=function(n){return function(r){return function(t){for(var o=r,e=t.length,a=0;a<e;a++)o=n(o)(t[a]);return o}}};var Sn=function(n){return n.foldr};var Vr=function(n){var r=Er(n.Apply0()),t=F(n);return function(o){var e=Sn(o);return function(a){return e(function(i){return r(a(i))})(t(void 0))}}},Hr=function(n){var r=Vr(n);return function(t){return gn(r(t))}};var he=function(n){var r=Sn(n);return function(t){var o=d(t.Semigroup0()),e=J(t);return function(a){return r(function(i){return function(u){return o(a(i))(u)}})(e)}}},fn={foldr:ce,foldl:fe,foldMap:function(n){return he(fn)(n)}};var qe=function(){function n(e){return[e]}function r(e){return function(a){return[e,a]}}function t(e){return function(a){return function(i){return[e,a,i]}}}function o(e){return function(a){return e.concat(a)}}return function(e){return function(a){return function(i){return function(u){return function(f){function x(w,mn){switch(mn-w){case 0:return i([]);case 1:return a(n)(u(f[w]));case 2:return e(a(r)(u(f[w])))(u(f[w+1]));case 3:return e(e(a(t)(u(f[w])))(u(f[w+1])))(u(f[w+2]));default:var Nn=w+Math.floor((mn-w)/4)*2;return e(a(o)(x(w,Nn)))(x(Nn,mn))}}return x(0,f.length)}}}}}}();var Fu=R(N),Kn=function(n){return n.traverse};var hu=function(n){var r=Kn(n);return function(t){return r(t)(Fu)}},Xn={traverse:function(n){var r=n.Apply0();return qe(U(r))(c(r.Functor0()))(F(n))},sequence:function(n){return hu(Xn)(n)},Functor0:function(){return on},Foldable1:function(){return fn}};var Oe=function(n){return function(r){return oe(Ur(r))(n)()}};var Pu=window.document;function Z(n){return function(){return window.document.getElementById(n)}}function nr(n){return function(r){return function(t){return function(){n.addEventListener(r,t)}}}}var rr=function(r){return function(t){return r(t)()}};var K=function(r){return function(t){return function(){return r(t)}}},tr=function(r){return function(t){return function(o){return function(){return r(t,o)}}}};var Ku=function(n){return n},er=function(n){return Ku(rr(n))};function On(n){return n.ownerDocument?.defaultView??window}function or(n){return n.ownerDocument??document}function nt(n){let r=On(n);return n instanceof r.HTMLElement}function Le(n){return n.nodeName==="HTML"}function rt(n){return n.nodeName==="BODY"}function ar(n){return n.documentElement}function tt(n){let r=ar(or(n));return Ne({width:r.clientWidth,height:r.clientHeight,top:0,left:0})}function ur(n){return n.getBoundingClientRect()}function et(n){let{width:r,height:t}=ur(n);return{width:r,height:t}}function ot(n){let{left:r,top:t}=n.getBoundingClientRect();return Ne({left:r,top:t,width:n.clientWidth,height:n.clientHeight})}function at(n){return{scrollLeft:n.scrollLeft,scrollTop:n.scrollTop}}function xn(n){return On(n).getComputedStyle(n)}function ir(n){if(!nt(n))return!1;let{display:r,overflow:t,overflowX:o,overflowY:e}=xn(n);if(["inline","contents"].includes(r))return!1;let a=["auto","scroll","overlay","hidden","clip"];return a.includes(t)||a.includes(o)||a.includes(e)}function cr(n){if(Le(n)||rt(n))return null;let r=n.parentElement;return ir(r)?r:cr(r)}function ut(n){return r([],n);function r(t,o){let e=cr(o);return e==null?t:r([...t,e],e)}}function it(n){let{filter:r,backdropFilter:t,transform:o,perspective:e,contain:a,containerType:i,willChange:u}=xn(n);return r!=="none"||t!=="none"||o!=="none"||e!=="none"||["layout","paint","strict","content"].includes(a)||i!=="normal"||["transform","perspective","filter"].includes(u)}function ct(n){if(!nt(n)||xn(n).position==="fixed")return null;let r=n.offsetParent;return r!=null&&rt(r)&&xn(r).position==="static"&&!it(r)?null:r}function Ne(n){return{...n,right:n.left+n.width,bottom:n.top+n.height}}function ft(n,r){let t=!1;n.addEventListener("scroll",o=>{t||(window.requestAnimationFrame(()=>{r(o),t=!1}),t=!0)})}function Be(n,r,t){return n==null?r:t(n)}var fr=function(n){return Be(n,D.value,y.create)};var ei=c(Dn),oi=Kn(Xn)(en),mt=tr(ft),je=K(ir);var Je=K(tt),vt=K(ut),We=function(){var n=ei(fr),r=K(ct);return function(t){return n(r(t))}}();var st=K(at),pr=K(ur),ai=K(ot),ze=function(n){return function(){var t=vt(n)();return oi(ai)(t)()}},Ge=K(et);function _t(){return{}}function lr(n){return function(r){return function(t){return function(){return t[n]=r,t}}}}function Ve(n){return function(){let r={};for(let[t,o]of Object.entries(n))r[t]=o;return r}}function dt(n){return n()}function He(n){return function(r){return function(t){return function(o){let e=t;function a(i){return function(u){return r(u)(i)(o[i])}}for(let i of Object.keys(o))e=n(e)(a(i));return e}}}}var mi=B(Vn),vi=$(Rn);var si=Ve;var Un=function(n){return function(r){return dt(mi(_t)(lr(n)(r)))}};var _i=function(n){return function(r){return dt(function(){var o=si(r)();return vi(n(o))(),o})}};var di=function(n){var r=B(n.Bind1()),t=F(n.Applicative0());return function(o){return function(e){return He(r)(o)(t(e))}}},Di=di($r),yi=function(n){return _i(function(r){return Di(function(t){return function(o){return function(e){return lr(o)(e)(t)}}})(r)(n)})},Ye={append:yi};function mr(n){return function(r){return function(){Object.assign(n,r)}}}function kn(n){return function(r){return n[r]}}function vr(n){return new Error(n)}function sr(n){return function(){throw n}}var Dt=function(n){return n()};var Ci=function(n){return Dt(sr(n))},Ke=function(n){return Ci(vr(n))};var Xe=function(n){var r=Sn(n);return function(t){return function(o){var e=function(i){return function(u){return new y(function(){if(u instanceof D)return i;if(u instanceof y)return t(i)(u.value0);throw new Error("Failed pattern match at Data.Foldable.Unsafe (line 39, column 20 - line 41, column 37): "+[u.constructor.name])}())}},a=r(e)(D.value)(o);if(a instanceof D)return Ke("foldr1: empty");if(a instanceof y)return a.value0;throw new Error("Failed pattern match at Data.Foldable.Unsafe (line 35, column 5 - line 37, column 21): "+[a.constructor.name])}}};var C=function(){function n(){}return n.value=new n,n}(),W=function(){function n(){}return n.value=new n,n}(),z=function(){function n(){}return n.value=new n,n}(),G=function(){function n(){}return n.value=new n,n}(),T=function(){function n(){}return n.value=new n,n}(),pn=function(){function n(){}return n.value=new n,n}(),ln=function(){function n(){}return n.value=new n,n}(),v=function(){function n(r,t){this.value0=r,this.value1=t}return n.create=function(r){return function(t){return new n(r,t)}},n}(),yt=function(n){return function(r){return{width:n.width,height:n.height,left:r.left,top:r.top,bottom:r.top+n.height,right:r.left+n.width}}},gt=function(n){return{left:n.left,top:n.top}},Ze=function(n){var r=function(){if(n.value0 instanceof C)return z.value;if(n.value0 instanceof z)return C.value;if(n.value0 instanceof G)return W.value;if(n.value0 instanceof W)return G.value;throw new Error("Failed pattern match at Popper.Type (line 21, column 19 - line 25, column 27): "+[n.value0.constructor.name])}();return new v(r,n.value1)};var Pi=Xe(fn),no=Ar(Wn),ro=Pr(Wn),ht=function(n){return function(r){return{width:r.width,height:r.height,top:r.top-n.top,bottom:r.bottom-n.top,left:r.left-n.left,right:r.right-n.left}}},bt=function(n){return function(r){return{width:r.width,height:r.height,top:r.top+n.scrollTop,bottom:r.bottom+n.scrollTop,left:r.left+n.scrollLeft,right:r.right+n.scrollLeft}}},to=function(n){return function(r){return function(t){var o=function(){return r.value0 instanceof C?t.top-n:r.value0 instanceof z?t.top+n:t.top}(),e=function(){return r.value0 instanceof G?t.left-n:r.value0 instanceof W?t.left+n:t.left}();return{left:e,top:o}}}},qi=function(n){return Pi(function(r){return function(t){var o=no(r.top)(t.top),e=ro(r.right)(t.right),a=no(r.left)(t.left),i=ro(r.bottom)(t.bottom);return{top:o,right:e,bottom:i,left:a,width:e-a,height:i-o}}})(n)},wt=function(n){return function(r){var t=qi(n);return{overflowTop:t.top-r.top,overflowBottom:r.bottom-t.bottom,overflowLeft:t.left-r.left,overflowRight:r.right-t.right}}},eo=function(n){return function(r){return function(t){var o=function(){if(t.value0 instanceof C)return n.top-r.height;if(t.value0 instanceof z)return n.bottom;if(t.value1 instanceof pn)return n.top;if(t.value1 instanceof ln)return n.bottom-r.height;if(t.value1 instanceof T)return n.top+n.height/2-r.height/2;throw new Error("Failed pattern match at Popper.Position (line 74, column 11 - line 79, column 74): "+[t.value0.constructor.name,t.value1.constructor.name])}(),e=function(){if(t.value0 instanceof G)return n.left-r.width;if(t.value0 instanceof W)return n.right;if(t.value1 instanceof pn)return n.left;if(t.value1 instanceof ln)return n.right-r.width;if(t.value1 instanceof T)return n.left+n.width/2-r.width/2;throw new Error("Failed pattern match at Popper.Position (line 68, column 12 - line 73, column 72): "+[t.value0.constructor.name,t.value1.constructor.name])}();return{left:e,top:o}}}};var oo=m(It),Ii=Tr(Qn),$i=F(en),Oi=c(Dn),ao=d(Ye),_r=function(){function n(r,t){this.value0=r,this.value1=t}return n.create=function(r){return function(t){return new n(r,t)}},n}(),uo=function(n){var r={scrollLeft:-n.scroll.scrollLeft,scrollTop:-n.scroll.scrollTop},t=gt(n.rect),o={left:-t.left,top:-t.top},e=bt(r),a=ht(o);return function(i){return e(a(i))}},Ui=function(n){var r=bt(n.scroll),t=ht(gt(n.rect));return function(o){return r(t(o))}},dr=function(n){return function(r){var t="translate("+(oo(r.left)+("px, "+(oo(r.top)+"px)")));return mr(kn(n)("style"))(Un("transform")(t))}},co=function(n){return function(r){var t=to(n)(r.value1.placement)(r.value0);return new _r(t,r.value1)}},Dr=function(n){return function(r){return function(){var o=Ge(r)(),e=function(){var u=We(r)();if(u instanceof D){var f=ar(or(r)),x=pr(f)(),w=Ii(je(f))(st(f))($i({scrollLeft:0,scrollTop:0}))();return{rect:x,scroll:w}}if(u instanceof y){var x=pr(u.value0)(),w=st(u.value0)();return{rect:x,scroll:w}}throw new Error("Failed pattern match at Popper.State (line 45, column 9 - line 57, column 36): "+[u.constructor.name])}(),a=Oi(Ui(e))(pr(n))();return{reference:{rect:a},float:{dimensions:o},containingBlock:e,placement:new v(C.value,T.value),overflowingRects:[]}}}},io=function(n){return function(r){if(n.value0 instanceof C)return r.overflowTop>0;if(n.value0 instanceof z)return r.overflowBottom>0;if(n.value0 instanceof G)return r.overflowLeft>0;if(n.value0 instanceof W)return r.overflowRight>0;throw new Error("Failed pattern match at Popper.State (line 131, column 5 - line 135, column 59): "+[n.constructor.name])}},yr=function(n){return mr(kn(n)("style"))(ao(Un("position")("absolute"))(ao(Un("top")("0"))(Un("left")("0"))))},fo=function(n){return function(r){var t=Ze(r.value1.placement),o=n({reference:r.value1.reference,float:r.value1.float,containingBlock:r.value1.containingBlock,overflowingRects:r.value1.overflowingRects,placement:t}),e=uo(r.value1.containingBlock)(yt(r.value1.float.dimensions)(r.value0)),a=io(r.value1.placement)(wt(r.value1.overflowingRects)(e)),i=uo(r.value1.containingBlock)(yt(r.value1.float.dimensions)(o.value0)),u=io(t)(wt(r.value1.overflowingRects)(i)),f=!a||u;return f?new _r(r.value0,r.value1):new _r(o.value0,o.value1)}},gr=function(n){return function(r){var t=n(r);return t.value0}},Ln=function(n){var r=eo(n.reference.rect)(n.float.dimensions)(n.placement);return new _r(r,n)};var Li=Hr(en)(fn),Ni=function(){var r=Z("case3_ref")(),t=Z("case3_float")();yr(t)();var o=vt(t)(),e=On(t),a=function(){var f=Dr(r)(t)(),x=ze(t)(),w=Je(t)(),mn=Oe(x)(w),Nn={containingBlock:f.containingBlock,float:f.float,reference:f.reference,placement:new v(C.value,T.value),overflowingRects:mn},lo=gr(function(){var mo=fo(Ln);return function(vo){return mo(Ln(vo))}}())(Nn);return dr(t)(lo)()},i=er(function(u){return a});return Li(o)(function(u){return mt(u)(i)})(),mt(e)(i)(),a()},Bi=function(){var r=Z("case2_ref")(),t=Z("case2_float")();yr(t)();var o=function(a){return function(){var u=Dr(r)(t)(),f={containingBlock:u.containingBlock,float:u.float,overflowingRects:u.overflowingRects,reference:u.reference,placement:a},x=gr(function(){var w=co(10);return function(mn){return w(Ln(mn))}}())(f);return dr(t)(x)()}},e=function(a){return function(i){return function(){var f=Z(a)();return nr(f)("click")(er(function(x){return o(i)}))()}}};return o(new v(C.value,T.value))(),e("case2_btn_t")(new v(C.value,T.value))(),e("case2_btn_r")(new v(W.value,T.value))(),e("case2_btn_b")(new v(z.value,T.value))(),e("case2_btn_l")(new v(G.value,T.value))()},ji=function(){var r=Z("case1_ref")(),t=Z("case1_float")();yr(t)();var o=function(a){return function(){var u=Dr(r)(t)(),f={containingBlock:u.containingBlock,float:u.float,overflowingRects:u.overflowingRects,reference:u.reference,placement:a},x=gr(Ln)(f);return dr(t)(x)()}},e=function(a){return function(i){return function(){var f=Z(a)();return nr(f)("click")(er(function(x){return o(i)}))()}}};return o(new v(C.value,T.value))(),e("case1_btn_ts")(new v(C.value,pn.value))(),e("case1_btn_t")(new v(C.value,T.value))(),e("case1_btn_te")(new v(C.value,ln.value))(),e("case1_btn_rs")(new v(W.value,pn.value))(),e("case1_btn_r")(new v(W.value,T.value))(),e("case1_btn_re")(new v(W.value,ln.value))(),e("case1_btn_bs")(new v(z.value,pn.value))(),e("case1_btn_b")(new v(z.value,T.value))(),e("case1_btn_be")(new v(z.value,ln.value))(),e("case1_btn_ls")(new v(G.value,pn.value))(),e("case1_btn_l")(new v(G.value,T.value))(),e("case1_btn_le")(new v(G.value,ln.value))()},po=function(){return ji(),Bi(),Ni()};po();})();