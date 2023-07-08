(html
  (head
    (style
      (css
	"html" (background-color "#222")
	"body" (margin "40px" font-size "18px")
	"#navbar" (overflow hidden padding-left "25%")
	"#navbar a" (float left color "#f2f2f2" text-align center
			   text-decoration none padding "10px 14px")
	"#navbar a:hover" (color "#ff5733" background-color "#222")
	"#main" (top-margin "10px" width "50%" padding "2px 20%")
	"#main div" (padding "0 8px"
		     width "100%"
		     color "#ccc" background-color "#444"
		     float left border "1px solid black" box-shadow "6px 6px 20px black")
	"a" (color "#ff5733" text-decoration none))))
  (body
    (div :attrs (id "navbar")
      (a :attrs (href "/")
	(b "home"))
      (a :attrs (href "/site-generator")
	(b "Site Generator")))
    (div :attrs (id "main")
      (div
	(process-markdown *markdown-file*)))))
