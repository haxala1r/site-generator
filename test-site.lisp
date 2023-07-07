(html
  (head
    (style
      (css
	html (background-color black)
	body (margin 0)
	"#main-topic" (text-align center color white width 60% float left)
        ".side-bar" (color white width 20% float left text-align center)
	"#navbar" (margin 0 overflow hidden background-color gray)
	"#navbar a" (padding 7px text-decoration none display block
		     color black float left)
	"#navbar a:hover" (background-color white))))
  (body
    (div :attrs (id "navbar")
      (a :attrs (href "./")
	"I am a LINK!")
      (a :attrs (href "https://rickroll.com")
	"We are FANCY and EFFING SCHMANCY. click me."))
    (br)
    (div :attrs (class "side-bar")
      (p
	(b "hello, i am a sidebar")))
    (div :attrs (id "main-topic")
      (h1 (b "Hello world!"))
      (p (b "Yes, I made this website using nothing but common lisp code to generate HTML. I find it quite satisfying
             to write fake html instead of real html... oh god what have I done.")))
    (div :attrs (class "side-bar")
      (p "oooh... me too!"))))
