;;; All Common Lisp packages that are actively depended on in the ecosystem.

(in-package :vend)

;; TODO: 2024-01-11 Make this a HashTable.
(defparameter +parents+
  '(:3bmd-ext-code-blocks :3bmd
    :3b-bmfont/json :3b-bmfont
    :alloy-animation :alloy
    :alloy-simple :alloy
    :alloy-simple-presentations :alloy
    :array-operations/all :array-operations
    :autoload :mgl-pax
    :automaton :mcclim
    :babel-streams :babel
    :binpack/2 :binpack
    :cl-aa :cl-vectors
    :cl-aa-misc :cl-vectors
    :cl-async-ssl :cl-async
    :cl-dbi :dbi
    :cl-gio :cl-glib
    :cl-glu :cl-opengl
    :cl-glut :cl-opengl
    :cl-gobject :cl-glib
    :cl-paths :cl-vectors
    :cl-paths-ttf :cl-vectors
    :cl-pdf-parser :cl-pdf
    :cl-plus-c :cl-autowrap
    :cl-postgres :postmodern
    :cl-postgres+local-time :local-time
    :cl-ppcre-unicode :cl-ppcre
    :cl-unicode/base :cl-unicode
    ;; NOTE: 2025-01-13 Breaking the normal pattern, this Clack-related system
    ;; actually lives in Woo.
    :clack-handler-woo :woo
    :clim-lisp :mcclim
    :clue :alive-lsp
    :coalton/hashtable-shim :coalton
    :depot-in-memory :depot
    :depot-virtual :depot
    :depot-zip :depot
    :dref :mgl-pax
    :eclector-concrete-syntax-tree :eclector
    :feedback-client :feedback
    :glsl-symbols :glsl-spec
    :glsl-docs :glsl-spec
    :lem-mailbox :lem-mailbox
    :list-of :asdf-finalizers
    :machine-state/opengl :machine-state
    :maxpc-apache :april
    :mcclim-layouts :mcclim
    :mgl-pax-bootstrap :mgl-pax
    :nasdf :nyxt
    :north-core :north
    :north-drakma :north
    :nsymbols/star :nsymbols
    :persistent :mcclim
    :plump-dom :plump
    :ql-dist :quicklisp
    :queues.priority-queue :queues
    :qtcore :qt+libs
    :qtgui :qt+libs
    :qtopengl :qt+libs
    :regression-test :ansi-test
    :rt :ansi-test
    :rtg-math.vari :rtg-math
    :sdf/bmfont :sdf
    :simple-date :postmodern
    :tiny-routes-middleware-cookie :tiny-routes
    ;; NOTE: 2025-01-11 This being here overrides the `trial-' mapping in
    ;; `get-parent' but it is necessary, as `trial-assets' is a separate repo.
    :trial-assets :trial-assets
    :trivia.balland2006 :trivia
    :trivia.level2 :trivia
    :trivia.trivial :trivia
    :ubiquitous-concurrent :ubiquitous
    :uiop :asdf)
  "Systems are often bundled together into a single repository. This list helps
map back to the parent, such that later only one git clone is performed.")

(defparameter +missing+
  '(:hu.dwim.presentation
    :hu.dwim.web-server
    :kmrcl
    :ptester)
  "Could not be found on a public repository.")

(defparameter +exclude+
  '(;; Compiler Internals
    :osi
    :sb-bsd-sockets
    :sb-cltl2
    :sb-concurrency
    :sb-cover
    :sb-gmp
    :sb-introspect
    :sb-md5
    :sb-mpfr
    :sb-posix
    :sb-sprof
    :sb-rotate-byte
    :syscalls
    :extensible-sequences
    :unix
    ;; Test systems
    :foo :foo/2
    ;; CFFI?
    :devil
    ;; A Corman-specific dependency requested in Bordeaux Threads.
    :threads
    ;; Way more trouble than its worth.)
    :asdf)
  "Known naughty systems that we can't do anything about.")

(defparameter +deprecated+
  '(:cl-annot
    :cl-colors
    :cl-colors2
    :cl-libyaml
    :cl-slug
    :cl-syntax
    :cl-yaml
    :dynamic-classes
    :prove
    :south
    :trivial-backtrace
    :trivial-open-browser
    :stefil)
  "Repositories marked as deprecated or archived by their authors.")

;; TODO: 2024-01-11 Make this a HashTable.
(defparameter +sources+
  '(:3b-bmfont       "https://github.com/3b/3b-bmfont.git"
    :3b-hdr          "https://github.com/3b/3b-hdr.git"
    :3bmd            "https://github.com/3b/3bmd.git"
    :3bz             "https://github.com/3b/3bz.git"
    :3d-math         "https://github.com/Shinmera/3d-math.git"
    :3d-spaces       "https://github.com/Shirakumo/3d-spaces.git"
    :40ants-doc      "https://github.com/40ants/doc.git"
    :access          "https://github.com/AccelerationNet/access.git"
    :acclimation     "https://github.com/robert-strandh/Acclimation.git"
    :action-list     "https://github.com/Shinmera/action-list.git"
    :adopt           "https://github.com/sjl/adopt.git"
    :alexandria      "https://gitlab.common-lisp.net/alexandria/alexandria.git"
    :alexandria+     "https://github.com/Symbolics/alexandria-plus.git"
    :alloy           "https://github.com/Shirakumo/alloy.git"
    :anaphora        "https://github.com/spwhitton/anaphora.git"
    :ansi-test       "https://gitlab.common-lisp.net/ansi-test/ansi-test.git"
    :anypool         "https://github.com/fukamachi/anypool"
    :arnesi          "https://github.com/AccelerationNet/arnesi.git"
    :array-operations "https://github.com/Lisp-Stat/array-operations.git"
    :array-utils     "https://github.com/Shinmera/array-utils.git"
    :arrow-macros    "https://github.com/hipeta/arrow-macros.git"
    :asdf            "https://gitlab.common-lisp.net/asdf/asdf.git"
    :asdf-finalizers "https://gitlab.common-lisp.net/asdf/asdf-finalizers.git"
    :asdf-release-ops "https://github.com/daewok/asdf-release-ops.git"
    :assoc-utils     "https://github.com/fukamachi/assoc-utils.git"
    :async-process   "https://github.com/lem-project/async-process.git"
    :atomics         "https://github.com/Shinmera/atomics.git"
    :babel           "https://github.com/cl-babel/babel.git"
    :blackbird       "https://github.com/orthecreedence/blackbird.git"
    :bobbin          "https://github.com/sjl/bobbin.git"
    :bodge-heap      "https://github.com/borodust/bodge-heap.git"
    :bordeaux-threads "https://github.com/sionescu/bordeaux-threads.git"
    :binary-structures "https://github.com/Shinmera/binary-structures.git"
    :binpack         "https://github.com/lispgames/binpack.git"
    :bt-semaphore    "https://github.com/r-moeritz/bt-semaphore.git"
    :calispel        "https://github.com/hawkir/calispel.git"
    :cephes          "https://github.com/Lisp-Stat/cephes.cl.git"
    :cffi            "https://github.com/cffi/cffi.git"
    :chanl           "https://github.com/zkat/chanl.git"
    :checkl          "https://github.com/rpav/CheckL.git"
    :chipz           "https://github.com/sharplispers/chipz.git"
    :chronicity      "https://github.com/chaitanyagupta/chronicity.git"
    :chunga          "https://github.com/edicl/chunga.git"
    :circular-streams "https://github.com/fukamachi/circular-streams.git"
    :cl+ssl          "https://github.com/cl-plus-ssl/cl-plus-ssl.git"
    :cl-autowrap     "https://github.com/rpav/cl-autowrap.git"
    :cl-annot        "https://github.com/m2ym/cl-annot.git"
    :cl-anonfun      "https://github.com/arielnetworks/cl-anonfun.git"
    :cl-ansi-text    "https://github.com/pnathan/cl-ansi-text.git"
    :cl-async        "https://github.com/orthecreedence/cl-async.git"
    :cl-base16       "https://github.com/tpine/cl-base16.git"
    :cl-base32       "https://github.com/hargettp/cl-base32.git"
    :cl-base64       "https://github.com/darabi/cl-base64.git"
    :cl-beanstalk    "https://github.com/antifuchs/cl-beanstalk.git"
    :cl-bmp          "https://github.com/Shinmera/cl-bmp.git"
    :cl-cffi-gtk     "https://github.com/sharplispers/cl-cffi-gtk.git"
    :cl-change-case  "https://github.com/rudolfochrist/cl-change-case.git"
    :cl-charms       "https://github.com/HiTECNOLOGYs/cl-charms.git"
    :cl-colors       "https://github.com/tpapp/cl-colors.git"
    :cl-colors2      "https://codeberg.org/cage/cl-colors2.git"
    :cl-colors-ng    "https://codeberg.org/cage/cl-colors-ng.git"
    :cl-containers   "https://github.com/hraban/cl-containers.git"
    :cl-cookie       "https://github.com/fukamachi/cl-cookie.git"
    :cl-coveralls    "https://github.com/fukamachi/cl-coveralls.git"
    :cl-csv          "https://github.com/AccelerationNet/cl-csv.git"
    :cl-store        "https://github.com/skypher/cl-store.git"
    :cl-cairo2       "https://github.com/rpav/cl-cairo2.git"
    :cl-custom-hash-table "https://github.com/metawilm/cl-custom-hash-table.git"
    :cl-dejavu       "https://codeberg.org/TurtleWare/cl-dejavu.git"
    :cl-dot          "https://github.com/michaelw/cl-dot.git"
    :cl-electron     "https://github.com/atlas-engineer/cl-electron.git"
    :cl-fad          "https://github.com/edicl/cl-fad.git"
    :cl-fbx          "https://github.com/Shirakumo/cl-fbx.git"
    :cl-fond         "https://github.com/Shirakumo/cl-fond.git"
    :cl-freetype2    "https://github.com/rpav/cl-freetype2.git"
    :cl-ftp          "https://github.com/pinterface/cl-ftp.git"
    :cl-gamepad      "https://github.com/Shirakumo/cl-gamepad.git"
    :cl-glib         "https://github.com/bohonghuang/cl-glib.git"
    :cl-gltf         "https://github.com/Shirakumo/cl-gltf.git"
    :cl-gobject-introspection "https://github.com/andy128k/cl-gobject-introspection.git"
    :cl-gobject-introspection-wrapper "https://github.com/bohonghuang/cl-gobject-introspection-wrapper.git"
    :cl-gog-galaxy   "https://github.com/Shinmera/cl-gog-galaxy.git"
    :cl-gopher       "https://github.com/knusbaum/cl-gopher.git"
    :cl-html5-parser "https://github.com/rotatef/cl-html5-parser.git"
    :cl-i18n         "https://codeberg.org/cage/cl-i18n.git"
    :cl-indentify    "https://github.com/yitzchak/cl-indentify.git"
    :cl-interpol     "https://github.com/edicl/cl-interpol.git"
    :cl-isaac        "https://github.com/thephoeron/cl-isaac.git"
    :cl-jpeg         "https://github.com/sharplispers/cl-jpeg.git"
    :cl-json         "https://github.com/sharplispers/cl-json.git"
    :cl-ktx          "https://github.com/Shinmera/cl-ktx.git"
    :cl-l10n         "https://gitlab.common-lisp.net/cl-l10n/cl-l10n.git"
    :cl-libuv        "https://github.com/orthecreedence/cl-libuv.git"
    :cl-libyaml      "https://github.com/eudoxia0/cl-libyaml.git"
    :cl-locale       "https://github.com/fukamachi/cl-locale.git"
    :cl-log          "https://github.com/nicklevine/cl-log.git"
    :cl-markdown     "https://gitlab.common-lisp.net/cl-markdown/cl-markdown.git"
    :cl-markless     "https://github.com/Shirakumo/cl-markless.git"
    :cl-markup       "https://github.com/arielnetworks/cl-markup.git"
    :cl-migemo       "https://github.com/snmsts/cl-migemo.git"
    :cl-mime         "https://github.com/40ants/cl-mime.git"
    :cl-mixed        "https://github.com/Shirakumo/cl-mixed.git"
    :cl-modio        "https://github.com/Shinmera/cl-modio.git"
    :cl-mpg123       "https://github.com/Shirakumo/cl-mpg123.git"
    :cl-mustache     "https://github.com/kanru/cl-mustache.git"
    :cl-mysql        "https://github.com/hackinghat/cl-mysql.git"
    :cl-opengl       "https://github.com/3b/cl-opengl.git"
    :cl-opus         "https://github.com/Shirakumo/cl-opus.git"
    :cl-package-locks "https://github.com/elliottjohnson/cl-package-locks.git"
    :cl-pass         "https://github.com/eudoxia0/cl-pass.git"
    :cl-pdf          "https://github.com/mbattyani/cl-pdf.git"
    :cl-ppcre        "https://github.com/edicl/cl-ppcre.git"
    :cl-prevalence   "https://github.com/40ants/cl-prevalence.git"
    :cl-qoa          "https://github.com/Shinmera/cl-qoa.git"
    :cl-qprint       "https://github.com/eugeneia/cl-qprint.git"
    :cl-qrencode     "https://github.com/jnjcc/cl-qrencode.git"
    :cl-readline     "https://github.com/vindarel/cl-readline.git"
    :cl-redis        "https://github.com/vseloved/cl-redis"
    :cl-resvg        "https://github.com/Shirakumo/cl-resvg.git"
    :cl-sendmail     "https://github.com/u-u-h/cl-sendmail.git"
    :cl-setlocale    "https://github.com/shamazmazum/cl-setlocale.git"
    :cl-spark        "https://github.com/tkych/cl-spark.git"
    :cl-speedy-queue "https://github.com/zkat/cl-speedy-queue.git"
    :cl-slug         "https://github.com/EuAndreh/cl-slug.git"
    :cl-smtp         "https://gitlab.common-lisp.net/cl-smtp/cl-smtp.git"
    :cl-steamworks   "https://github.com/Shinmera/cl-steamworks.git"
    :cl-syntax       "https://github.com/m2ym/cl-syntax.git"
    :cl-template     "https://github.com/alpha123/cl-template.git"
    :cl-tga          "https://github.com/fisxoj/cl-tga.git"
    :cl-theora       "https://github.com/Shirakumo/cl-theora.git"
    :cl-tld          "https://github.com/lu4nx/cl-tld.git"
    :cl-transmission "https://github.com/libre-man/cl-transmission.git"
    :cl-turbojpeg    "https://github.com/Shirakumo/cl-turbojpeg.git"
    :cl-unicode      "https://github.com/edicl/cl-unicode.git"
    :cl-utilities    "https://gitlab.common-lisp.net/cl-utilities/cl-utilities.git"
    :cl-vectors      "https://github.com/fjolliton/cl-vectors.git"
    :cl-vorbis       "https://github.com/Shirakumo/cl-vorbis.git"
    :cl-wavefront    "https://github.com/Shirakumo/cl-wavefront.git"
    :cl-webkit2      "https://github.com/joachifm/cl-webkit.git"
    :cl-who          "https://github.com/edicl/cl-who.git"
    :cl-yaml         "https://github.com/eudoxia0/cl-yaml.git"
    :clache          "https://github.com/html/clache.git"
    :clack           "https://github.com/fukamachi/clack.git"
    :classowary      "https://github.com/Shinmera/classowary.git"
    :clavier         "https://github.com/mmontone/clavier.git"
    :clip            "https://github.com/Shinmera/clip.git"
    :clj-arrows      "https://github.com/dtenny/clj-arrows.git"
    :clj-coll        "https://github.com/dtenny/clj-coll.git"
    :clog-ace        "https://github.com/rabbibotton/clog-ace.git"
    :clog-terminal   "https://github.com/rabbibotton/clog-terminal.git"
    :clon            "https://github.com/didierverna/clon.git"
    :clop            "https://github.com/sheepduke/clop.git"
    :closer-mop      "https://github.com/pcostanza/closer-mop.git"
    :closure-common  "https://github.com/sharplispers/closure-common.git"
    :clsql           "https://github.com/UnwashedMeme/clsql.git"
    :clss            "https://github.com/Shinmera/CLSS.git"
    :clump           "https://github.com/robert-strandh/Clump.git"
    :clunit2         "https://codeberg.org/cage/clunit2.git"
    :cluffer         "https://github.com/robert-strandh/Cluffer.git"
    :clx             "https://github.com/sharplispers/clx.git"
    :cocoas          "https://github.com/Shinmera/cocoas.git"
    :collectors      "https://github.com/AccelerationNet/collectors.git"
    :colored         "https://github.com/Shinmera/colored.git"
    :colorize        "https://github.com/kingcons/colorize.git"
    :com.gigamonkeys.binary-data "https://github.com/gigamonkey/monkeylib-binary-data.git"
    :com.inuoe.jzon  "https://github.com/Zulu-Inuoe/jzon.git"
    :com-on          "https://github.com/Shinmera/com-on.git"
    :command-line-arguments "https://github.com/fare/command-line-arguments.git"
    :common-lisp-jupyter "https://github.com/yitzchak/common-lisp-jupyter.git"
    :computable-reals "https://github.com/stylewarning/computable-reals.git"
    :concrete-syntax-tree "https://github.com/s-expressionists/Concrete-Syntax-Tree.git"
    :contextl        "https://github.com/pcostanza/contextl.git"
    :convex-covering "https://github.com/Shirakumo/convex-covering.git"
    :croatoan        "https://codeberg.org/McParen/croatoan.git"
    :crypto-shortcuts "https://github.com/Shinmera/crypto-shortcuts.git"
    :cxml            "https://github.com/sharplispers/cxml.git"
    :damn-fast-priority-queue "https://github.com/phoe/damn-fast-priority-queue.git"
    :data-frame      "https://github.com/Lisp-Stat/data-frame.git"
    :db3             "https://github.com/dimitri/cl-db3.git"
    :dbi             "https://github.com/fukamachi/cl-dbi.git"
    :definitions     "https://github.com/Shinmera/definitions.git"
    :deflate         "https://github.com/pmai/Deflate.git"
    :defpackage-plus "https://github.com/rpav/defpackage-plus.git"
    :deploy          "https://github.com/Shinmera/deploy.git"
    :depot           "https://github.com/Shinmera/depot.git"
    :dexador         "https://github.com/fukamachi/dexador.git"
    :dfio            "https://github.com/Lisp-Stat/dfio.git"
    :distributions   "https://github.com/Lisp-Stat/distributions.git"
    :dns-client      "https://github.com/Shinmera/dns-client.git"
    :dissect         "https://github.com/Shinmera/dissect.git"
    :do-urlencode    "https://github.com/drdo/do-urlencode.git"
    :documentation-utils "https://github.com/Shinmera/documentation-utils.git"
    :drakma          "https://github.com/edicl/drakma.git"
    :duologue        "https://github.com/mmontone/duologue.git"
    :dynamic-classes "https://github.com/hraban/dynamic-classes.git"
    :easing          "https://github.com/vydd/easing.git"
    :eclector        "https://github.com/s-expressionists/Eclector.git"
    :enchant         "https://github.com/tlikonen/cl-enchant.git"
    :esrap           "https://github.com/scymtym/esrap.git"
    :external-program "https://github.com/sellout/external-program.git"
    :event-emitter   "https://github.com/fukamachi/event-emitter.git"
    :fare-csv        "https://gitlab.common-lisp.net/frideau/fare-csv.git"
    :fare-quasiquote "https://gitlab.common-lisp.net/frideau/fare-quasiquote.git"
    :fare-utils      "https://gitlab.common-lisp.net/frideau/fare-utils.git"
    :fast-http       "https://github.com/fukamachi/fast-http.git"
    :fast-io         "https://github.com/fosskers/fast-io.git"
    :fast-websocket  "https://github.com/fukamachi/fast-websocket.git"
    :feedback        "https://github.com/Shirakumo/feedback.git"
    :fiasco          "https://github.com/joaotavora/fiasco.git"
    :file-attributes "https://github.com/Shinmera/file-attributes.git"
    :file-notify     "https://github.com/Shinmera/file-notify.git"
    :file-select     "https://github.com/Shinmera/file-select.git"
    :filepaths       "https://github.com/fosskers/filepaths.git"
    :filesystem-utils "https://github.com/Shinmera/filesystem-utils.git"
    :fiveam          "https://github.com/lispci/fiveam.git"
    :flexi-streams   "https://github.com/edicl/flexi-streams.git"
    :flexichain      "https://github.com/robert-strandh/Flexichain.git"
    :float-features  "https://github.com/Shinmera/float-features.git"
    :flow            "https://github.com/Shinmera/flow.git"
    :fn              "https://github.com/cbaggers/fn.git"
    :fn-macro        "https://github.com/fosskers/fn-macro.git"
    :font-discovery  "https://github.com/Shinmera/font-discovery.git"
    :for             "https://github.com/Shinmera/for.git"
    :form-fiddle     "https://github.com/Shinmera/form-fiddle.git"
    :framebuffers    "https://github.com/Shirakumo/framebuffers.git"
    :frugal-uuid     "https://github.com/ak-coram/cl-frugal-uuid.git"
    :fset            "https://gitlab.common-lisp.net/fset/fset.git"
    :garbage-pools   "https://github.com/archimag/garbage-pools.git"
    :gettext         "https://github.com/rotatef/gettext.git"
    :glfw            "https://github.com/Shirakumo/glfw.git"
    :glkit           "https://github.com/lispgames/glkit.git"
    :global-vars     "https://github.com/lmj/global-vars.git"
    :glop            "https://github.com/lispgames/glop.git"
    :glsl-spec       "https://github.com/cbaggers/glsl-spec.git"
    :glsl-toolkit    "https://github.com/Shirakumo/glsl-toolkit.git"
    :glu-tessellate  "https://github.com/orthecreedence/glu-tessellate.git"
    :harmony         "https://github.com/Shirakumo/harmony.git"
    :history-tree    "https://github.com/atlas-engineer/history-tree.git"
    :hsx             "https://github.com/skyizwhite/hsx.git"
    :html-encode     "https://github.com/fosskers/html-encode.git"
    :html-entities   "https://github.com/BnMcGn/html-entities.git"
    :http-body       "https://github.com/fukamachi/http-body.git"
    :hu.dwim.common  "https://github.com/hu-dwim/hu.dwim.common.git"
    :hu.dwim.common-lisp "https://github.com/hu-dwim/hu.dwim.common-lisp.git"
    :hu.dwim.def     "https://github.com/hu-dwim/hu.dwim.def.git"
    :hu.dwim.defclass-star "https://github.com/hu-dwim/hu.dwim.defclass-star.git"
    :hu.dwim.delico  "https://github.com/hu-dwim/hu.dwim.delico.git"
    :hu.dwim.logger  "https://github.com/hu-dwim/hu.dwim.logger.git"
    :hu.dwim.partial-eval "https://github.com/hu-dwim/hu.dwim.partial-eval.git"
    :hu.dwim.stefil  "https://github.com/hu-dwim/hu.dwim.stefil.git"
    :hu.dwim.syntax-sugar "https://github.com/hu-dwim/hu.dwim.syntax-sugar.git"
    :hu.dwim.util    "https://github.com/hu-dwim/hu.dwim.util.git"
    :hu.dwim.walker  "https://github.com/hu-dwim/hu.dwim.walker.git"
    :hunchensocket   "https://github.com/joaotavora/hunchensocket.git"
    :hunchentoot     "https://github.com/edicl/hunchentoot.git"
    :iconv           "https://github.com/quek/cl-iconv.git"
    :ieee-floats     "https://github.com/marijnh/ieee-floats.git"
    :idna            "https://github.com/antifuchs/idna.git"
    :in-nomine       "https://github.com/phoe/in-nomine.git"
    :introspect-environment "https://github.com/Bike/introspect-environment.git"
    :iolib           "https://github.com/sionescu/iolib.git"
    :inquisitor      "https://github.com/t-sin/inquisitor.git"
    :ironclad        "https://github.com/sharplispers/ironclad.git"
    :iterate         "https://gitlab.common-lisp.net/iterate/iterate.git"
    :ixf             "https://github.com/dimitri/cl-ixf.git"
    :jonathan        "https://github.com/rudolph-miller/jonathan.git"
    :jpeg-turbo      "https://github.com/shamazmazum/jpeg-turbo.git"
    ;; NOTE: 2025-01-12 These `jpl' libs are dubious! They are both mirrors
    ;; since the originals aren't hosted anywhere.
    :jpl-queues      "https://github.com/jaredkrinke/jpl-queues.git"
    :jpl-util        "https://github.com/hawkir/cl-jpl-util.git"
    :jsonrpc         "https://github.com/cxxxr/jsonrpc.git"
    :jsown           "https://github.com/madnificent/jsown.git"
    :lack            "https://github.com/fukamachi/lack.git"
    :lambda-fiddle   "https://github.com/Shinmera/lambda-fiddle.git"
    :lambdalite      "https://github.com/Wukix/LambdaLite.git"
    :language-codes  "https://github.com/Shinmera/language-codes.git"
    :lass            "https://github.com/Shinmera/LASS.git"
    :lem             "https://github.com/lem-project/lem.git"
    :lem-mailbox     "https://github.com/lem-project/lem-mailbox.git"
    :let-plus        "https://github.com/sharplispers/let-plus.git"
    :lev             "https://github.com/fukamachi/lev.git"
    :lift            "https://github.com/hraban/lift.git"
    :lisp-binary     "https://github.com/j3pic/lisp-binary.git"
    :lisp-namespace  "https://github.com/guicho271828/lisp-namespace.git"
    :lisp-preprocessor "https://github.com/cxxxr/lisp-preprocessor.git"
    :lisp-unit       "https://github.com/OdonataResearchLLC/lisp-unit.git"
    :lisp-unit2      "https://github.com/AccelerationNet/lisp-unit2.git"
    :lispqr          "https://github.com/mare5x/LispQR.git"
    :local-time      "https://github.com/dlowe-net/local-time.git"
    :log4cl          "https://github.com/sharplispers/log4cl.git"
    :lorem-ipsum     "https://github.com/phoe/lorem-ipsum.git"
    :lparallel       "https://github.com/lmj/lparallel.git"
    :lquery          "https://github.com/Shinmera/lquery.git"
    :lru-cache       "https://github.com/Shinmera/lru-cache.git"
    :lsx             "https://github.com/fukamachi/lsx.git"
    :ltk             "https://github.com/ghollisjr/ltk.git"
    :machine-state   "https://github.com/Shinmera/machine-state.git"
    :magicffi        "https://github.com/guicho271828/magicffi.git"
    :manifolds       "https://github.com/Shirakumo/manifolds.git"
    :map-set         "https://github.com/stylewarning/map-set.git"
    :markup          "https://github.com/moderninterpreters/markup.git"
    :marshal         "https://github.com/wlbr/cl-marshal.git"
    :mathkit         "https://github.com/lispgames/mathkit.git"
    :maxpc           "https://github.com/eugeneia/maxpc.git"
    :mcclim          "https://codeberg.org/McCLIM/McCLIM.git"
    :md5             "https://github.com/pmai/md5.git"
    :memory-regions  "https://github.com/Shinmera/memory-regions.git"
    :messagebox      "https://github.com/Shinmera/messagebox.git"
    :metabang-bind   "https://github.com/hraban/metabang-bind.git"
    :metatilities-base "https://github.com/hraban/metatilities-base.git"
    :mgl-pax         "https://github.com/melisgl/mgl-pax.git"
    :micros          "https://github.com/lem-project/micros.git"
    :minheap         "https://github.com/sfrank/minheap.git"
    :misc-extensions "https://gitlab.common-lisp.net/misc-extensions/misc-extensions.git"
    :mito            "https://github.com/fukamachi/mito.git"
    :mmap            "https://github.com/Shinmera/mmap.git"
    :mockingbird     "https://github.com/pfdietz/mockingbird.git"
    :modularize      "https://github.com/Shinmera/modularize.git"
    :modularize-hooks "https://github.com/Shinmera/modularize-hooks.git"
    :modularize-interfaces "https://github.com/Shinmera/modularize-interfaces.git"
    :moptilities     "https://github.com/hraban/moptilities.git"
    :mssql           "https://github.com/archimag/cl-mssql.git"
    :mt19937         "https://github.com/DruidGreeneyes/MT19937.git"
    :multilang-documentation "https://github.com/Shinmera/multilang-documentation.git"
    :mystic          "https://github.com/roswell/mystic.git"
    :myway           "https://github.com/fukamachi/myway.git"
    :named-readtables "https://github.com/melisgl/named-readtables.git"
    :nclasses        "https://github.com/atlas-engineer/nclasses.git"
    :ndebug          "https://github.com/atlas-engineer/ndebug.git"
    :net.didierverna.asdf-flv "https://github.com/didierverna/asdf-flv.git"
    :nfiles          "https://github.com/atlas-engineer/nfiles.git"
    :nhooks          "https://github.com/atlas-engineer/nhooks.git"
    :njson           "https://github.com/atlas-engineer/njson.git"
    :nkeymaps        "https://github.com/atlas-engineer/nkeymaps.git"
    :nibbles         "https://github.com/sharplispers/nibbles.git"
    :nodgui          "https://codeberg.org/cage/nodgui.git"
    :north           "https://github.com/Shinmera/north.git"
    :nonempty        "https://codeberg.org/fosskers/nonempty.git"
    :nontrivial-gray-streams "https://github.com/yitzchak/nontrivial-gray-streams.git"
    :nsymbols        "https://github.com/atlas-engineer/nsymbols.git"
    :num-utils       "https://github.com/Lisp-Stat/numerical-utilities.git"
    :nyxt            "https://github.com/atlas-engineer/nyxt.git"
    :open-with       "https://github.com/Shinmera/open-with.git"
    :opticl          "https://github.com/slyrus/opticl.git"
    :opticl-core     "https://github.com/slyrus/opticl-core.git"
    :optima          "https://github.com/m2ym/optima.git"
    :org.tfeb.conduit-packages "https://github.com/tfeb/conduit-packages.git"
    :osicat          "https://github.com/osicat/osicat.git"
    :parachute       "https://github.com/Shinmera/parachute.git"
    :parcom          "https://github.com/fosskers/parcom.git"
    :parenscript     "https://gitlab.common-lisp.net/parenscript/parenscript.git"
    :parse-declarations-1.0 "https://gitlab.common-lisp.net/parse-declarations/parse-declarations.git"
    :parse-float     "https://github.com/soemraws/parse-float.git"
    :parse-number    "https://github.com/sharplispers/parse-number.git"
    :pathname-utils  "https://github.com/Shinmera/pathname-utils.git"
    :pettomato-indexed-priority-queue "https://github.com/austinhaas/pettomato-indexed-priority-queue.git"
    :percent-encoding "https://github.com/llibra/percent-encoding.git"
    :phos            "https://github.com/omar-polo/phos.git"
    :plump           "https://github.com/Shinmera/plump.git"
    :pileup          "https://github.com/nikodemus/pileup.git"
    :piping          "https://github.com/Shinmera/piping.git"
    :png-read        "https://github.com/Ramarren/png-read.git"
    :pngload         "https://github.com/3b/pngload.git"
    :postmodern      "https://github.com/marijnh/Postmodern.git"
    :precise-time    "https://github.com/Shinmera/precise-time.git"
    :print-licenses  "https://github.com/vindarel/print-licenses.git"
    :priority-queue  "https://github.com/dsorokin/priority-queue.git"
    :proc-parse      "https://github.com/fukamachi/proc-parse.git"
    :promise         "https://github.com/Shinmera/promise.git"
    :prompter        "https://github.com/atlas-engineer/prompter.git"
    :prove           "https://github.com/fukamachi/prove.git"
    :purgatory       "https://codeberg.org/cage/purgatory.git"
    :punycode        "https://github.com/Shinmera/punycode.git"
    :puri            "https://github.com/macrologist/puri.git"
    :py-configparser "https://github.com/mtstickney/py-configparser.git"
    :pythonic-string-reader "https://github.com/smithzvk/pythonic-string-reader.git"
    :pzmq            "https://github.com/orivej/pzmq.git"
    :qbase64         "https://github.com/chaitanyagupta/qbase64.git"
    :qlot            "https://github.com/fukamachi/qlot.git"
    :qmynd           "https://github.com/qitab/qmynd.git"
    :qoi             "https://github.com/bpanthi977/qoi.git"
    :quasiquote-2.0  "https://github.com/mabragor/quasiquote-2.0.git"
    :queues          "https://github.com/oconnore/queues.git"
    :quri            "https://github.com/fukamachi/quri.git"
    :qt+libs         "https://github.com/commonqt/commonqt.git"
    :qt-libs         "https://github.com/Shinmera/qt-libs.git"
    :qtools          "https://github.com/Shinmera/qtools.git"
    :quickhull       "https://github.com/Shirakumo/quickhull.git"
    :quicklisp       "https://github.com/quicklisp/quicklisp-client.git"
    :radiance        "https://github.com/Shirakumo/radiance.git"
    :radiance-contrib "https://github.com/Shirakumo/radiance-contribs.git"
    :random-sampling "https://github.com/Shinmera/random-sampling.git"
    :random-state    "https://github.com/Shinmera/random-state.git"
    :raster          "https://github.com/Shirakumo/raster.git"
    :ratify          "https://github.com/Shinmera/ratify.git"
    :raylib          "https://github.com/fosskers/raylib.git"
    :read-line-crlf  "https://github.com/dtenny/read-line-crlf.git"
    :replic          "https://github.com/vindarel/replic.git"
    :retrospectiff   "https://github.com/slyrus/retrospectiff.git"
    :rfc2388         "https://github.com/jdz/rfc2388.git"
    :rove            "https://github.com/fukamachi/rove.git"
    :rtg-math        "https://github.com/cbaggers/rtg-math.git"
    :rutils          "https://github.com/fosskers/rutils.git"
    :s-base64        "https://github.com/svenvc/s-base64.git"
    :s-sysdeps       "https://github.com/svenvc/s-sysdeps.git"
    :s-xml           "https://gitlab.common-lisp.net/s-xml/s-xml.git"
    :salza2          "https://github.com/xach/salza2.git"
    :safety-params   "https://github.com/fukamachi/safety-params.git"
    :sb-cga          "https://github.com/nikodemus/sb-cga.git"
    :sdf             "https://github.com/lispgames/sdf.git"
    :sdl2            "https://github.com/lispgames/cl-sdl2.git"
    :sdl2kit         "https://github.com/lispgames/sdl2kit.git"
    :sdl2-image      "https://github.com/lispgames/cl-sdl2-image.git"
    :sdl2-ttf        "https://github.com/sharplispers/cl-sdl2-ttf.git"
    :select          "https://github.com/Lisp-Stat/select.git"
    :serapeum        "https://github.com/ruricolist/serapeum.git"
    :sha1            "https://github.com/massung/sha1.git"
    :sha3            "https://github.com/pmai/sha3.git"
    :shasht          "https://github.com/yitzchak/shasht.git"
    :shlex           "https://github.com/ruricolist/cl-shlex.git"
    :si-kanren       "https://github.com/rgc69/si-kanren.git"
    :simple-date-time "https://github.com/quek/simple-date-time.git"
    :simple-graph    "https://github.com/fosskers/simple-graph.git"
    :simple-tasks    "https://github.com/Shinmera/simple-tasks.git"
    :skippy          "https://github.com/xach/skippy.git"
    :slynk           "https://github.com/joaotavora/sly.git"
    :smart-buffer    "https://github.com/fukamachi/smart-buffer.git"
    :south           "https://github.com/Shinmera/south.git"
    :spatial-trees   "https://github.com/rpav/spatial-trees.git"
    :special-functions "https://github.com/Lisp-Stat/special-functions.git"
    :speechless      "https://github.com/Shirakumo/speechless.git"
    :split-sequence  "https://github.com/sharplispers/split-sequence.git"
    :spinneret       "https://github.com/ruricolist/spinneret.git"
    :stefil          "https://gitlab.common-lisp.net/stefil/stefil.git"
    :sqlite          "https://github.com/TeMPOraL/cl-sqlite.git"
    :static-vectors  "https://github.com/sionescu/static-vectors.git"
    :statistics      "https://github.com/Lisp-Stat/statistics.git"
    :stealth-mixin   "https://github.com/robert-strandh/Stealth-mixin.git"
    :str             "https://github.com/vindarel/cl-str.git"
    :string-case     "https://github.com/pkhuong/string-case.git"
    :swank           "https://github.com/slime/slime.git"
    :swap-bytes      "https://github.com/sionescu/swap-bytes.git"
    :sxql            "https://github.com/fukamachi/sxql.git"
    :sxql-composer   "https://github.com/mmontone/sxql-composer.git"
    :symbol-munger   "https://github.com/AccelerationNet/symbol-munger.git"
    :system-locale   "https://github.com/Shinmera/system-locale.git"
    :ten             "https://github.com/mmontone/ten.git"
    :terrable        "https://github.com/Shirakumo/terrable.git"
    :text-draw       "https://github.com/Shinmera/text-draw.git"
    :tooter          "https://github.com/Shinmera/tooter.git"
    :tiny-routes     "https://github.com/jeko2000/tiny-routes.git"
    :transducers     "https://github.com/fosskers/cl-transducers.git"
    :translate       "https://gitlab.common-lisp.net/dkochmanski/translate.git"
    :translate-client "https://github.com/aarvid/translate-client.git"
    :trial           "https://github.com/Shirakumo/trial.git"
    :trial-assets    "https://github.com/Shirakumo/trial-assets.git"
    :trivia          "https://github.com/guicho271828/trivia.git"
    :trivial-arguments "https://github.com/Shinmera/trivial-arguments.git"
    :trivial-backtrace "https://github.com/hraban/trivial-backtrace.git"
    :trivial-battery "https://github.com/pokepay/trivial-battery.git"
    :trivial-benchmark "https://github.com/Shinmera/trivial-benchmark.git"
    :trivial-channels "https://github.com/rpav/trivial-channels.git"
    :trivial-clipboard "https://github.com/snmsts/trivial-clipboard.git"
    :trivial-clock   "https://github.com/ak-coram/cl-trivial-clock.git"
    :trivial-cltl2   "https://github.com/Zulu-Inuoe/trivial-cltl2.git"
    :trivial-custom-debugger "https://github.com/phoe/trivial-custom-debugger.git"
    :trivial-deprecate "https://github.com/Shinmera/trivial-deprecate.git"
    :trivial-do      "https://github.com/yitzchak/trivial-do.git"
    :trivial-extensible-sequences "https://github.com/Shinmera/trivial-extensible-sequences.git"
    :trivial-garbage "https://github.com/trivial-garbage/trivial-garbage.git"
    :trivial-gray-streams "https://github.com/trivial-gray-streams/trivial-gray-streams.git"
    :trivial-features "https://github.com/trivial-features/trivial-features.git"
    :trivial-file-size "https://github.com/ruricolist/trivial-file-size.git"
    :trivial-indent  "https://github.com/Shinmera/trivial-indent.git"
    :trivial-ldap    "https://github.com/rwiker/trivial-ldap.git"
    :trivial-main-thread "https://github.com/Shinmera/trivial-main-thread.git"
    :trivial-macroexpand-all "https://github.com/cbaggers/trivial-macroexpand-all.git"
    :trivial-mimes   "https://github.com/Shinmera/trivial-mimes.git"
    :trivial-octet-streams "https://github.com/sharplispers/trivial-octet-streams.git"
    :trivial-open-browser "https://github.com/eudoxia0/trivial-open-browser.git"
    :trivial-package-local-nicknames "https://github.com/phoe/trivial-package-local-nicknames.git"
    :trivial-rfc-1123 "https://github.com/stacksmith/trivial-rfc-1123.git"
    :trivial-sanitize "https://codeberg.org/cage/trivial-sanitize.git"
    :trivial-shell   "https://github.com/hraban/trivial-shell.git"
    :trivial-ssh     "https://github.com/eudoxia0/trivial-ssh.git"
    :trivial-sockets "https://github.com/usocket/trivial-sockets.git"
    :trivial-timeout "https://github.com/hraban/trivial-timeout.git"
    :trivial-types   "https://github.com/m2ym/trivial-types.git"
    :trivial-utf-8   "https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8.git"
    :trivial-with-current-source-form "https://github.com/scymtym/trivial-with-current-source-form.git"
    :trivial-ws      "https://github.com/ceramic/trivial-ws.git"
    :try             "https://github.com/melisgl/try.git"
    :type-i          "https://github.com/guicho271828/type-i.git"
    :type-templates  "https://github.com/Shinmera/type-templates.git"
    :uax-14          "https://github.com/Shinmera/uax-14.git"
    :uax-15          "https://github.com/sabracrolleton/uax-15.git"
    :ubiquitous      "https://github.com/Shinmera/ubiquitous.git"
    :unit-test       "https://github.com/hanshuebner/unit-test.git"
    :unix-opts       "https://github.com/libre-man/unix-opts.git"
    :usocket         "https://github.com/usocket/usocket.git"
    :uuid            "https://github.com/dardoria/uuid.git"
    :varjo           "https://github.com/cbaggers/varjo.git"
    :vas-string-metrics "https://gitlab.common-lisp.net/vsedach/vas-string-metrics.git"
    :verbose         "https://github.com/Shinmera/verbose.git"
    :vom             "https://github.com/orthecreedence/vom.git"
    :websocket-driver "https://github.com/fukamachi/websocket-driver.git"
    :winhttp         "https://github.com/fjames86/winhttp.git"
    :with-user-abort "https://github.com/compufox/with-user-abort.git"
    :woo             "https://github.com/fukamachi/woo.git"
    :wookie          "https://github.com/orthecreedence/wookie.git"
    :wu-sugar        "https://github.com/Wukix/wu-sugar.git"
    :x.let-star      "https://github.com/ks/X.LET-STAR.git"
    :xmls            "https://github.com/rpgoldman/xmls.git"
    :xmls-tools      "https://github.com/rpgoldman/xmls-tools.git"
    :xsubseq         "https://github.com/fukamachi/xsubseq.git"
    :yacc            "https://github.com/jech/cl-yacc.git"
    :yason           "https://github.com/phmarek/yason.git"
    :zlib            "https://github.com/sharplispers/zlib.git"
    :zip             "https://github.com/bluelisp/zip.git"
    :zippy           "https://github.com/Shinmera/zippy.git"
    :zpb-exif        "https://github.com/xach/zpb-exif.git"
    :zpb-ttf         "https://github.com/xach/zpb-ttf.git"
    :zpng            "https://github.com/xach/zpng.git"
    :zs3             "https://github.com/xach/zs3.git")
  "All actively depended-on Common Lisp libraries.")

#++
(/ (length +sources+) 2)

;; The longest system name.
#++
(t:transduce (t:comp (t:segment 2)
                     (t:map #'car)
                     (t:map #'symbol-name)
                     (t:map #'length))
             (t:fold #'max)
             +sources+)

(defun get-parent (sys)
  (or (getf +parents+ sys)
      (when (string-starts-with? (symbol-name sys) "40ANTS-DOC-") :40ants-doc)
      (when (string-starts-with? (symbol-name sys) "40ANTS-DOC/") :40ants-doc)
      (when (string-starts-with? (symbol-name sys) "ALLOY-") :alloy)
      (when (string-starts-with? (symbol-name sys) "CL-MARKLESS-") :cl-markless)
      (when (string-starts-with? (symbol-name sys) "CL-MIXED-") :cl-mixed)
      (when (string-starts-with? (symbol-name sys) "CL-SYNTAX-") :cl-syntax)
      (when (string-starts-with? (symbol-name sys) "CFFI-") :cffi)
      (when (string-starts-with? (symbol-name sys) "CLACK-") :clack)
      (when (string-starts-with? (symbol-name sys) "DBD-") :dbi)
      (when (string-starts-with? (symbol-name sys) "FRUGAL-UUID/") :frugal-uuid)
      (when (string-starts-with? (symbol-name sys) "HSX/") :hsx)
      (when (string-starts-with? (symbol-name sys) "LSX/") :lsx)
      (when (string-starts-with? (symbol-name sys) "IOLIB/") :iolib)
      (when (string-starts-with? (symbol-name sys) "IRONCLAD/") :ironclad)
      (when (string-starts-with? (symbol-name sys) "JSONRPC/") :jsonrpc)
      (when (string-starts-with? (symbol-name sys) "LACK-") :lack)
      (when (string-starts-with? (symbol-name sys) "LEM-") :lem)
      (when (string-starts-with? (symbol-name sys) "LEV-") :lev)
      (when (string-starts-with? (symbol-name sys) "MEMORY-REGIONS/") :memory-regions)
      (when (string-starts-with? (symbol-name sys) "MOCKINGBIRD/") :mockingbird)
      (when (string-starts-with? (symbol-name sys) "MYSTIC-") :mystic)
      (when (string-starts-with? (symbol-name sys) "NJSON/") :njson)
      (when (string-starts-with? (symbol-name sys) "NYXT/") :nyxt)
      (when (string-starts-with? (symbol-name sys) "PARCOM/") :parcom)
      (when (string-starts-with? (symbol-name sys) "PROVE-") :prove)
      (when (string-starts-with? (symbol-name sys) "QLOT-TESTS/") :qlot)
      (when (string-starts-with? (symbol-name sys) "QLOT/") :qlot)
      (when (string-starts-with? (symbol-name sys) "QUEUES.") :queues)
      (when (string-starts-with? (symbol-name sys) "R-") :radiance-contrib)
      (when (string-starts-with? (symbol-name sys) "ROVE/") :rove)
      (when (string-starts-with? (symbol-name sys) "SLYNK/") :slynk)
      (when (string-starts-with? (symbol-name sys) "SWANK/") :swank)
      (when (string-starts-with? (symbol-name sys) "TRANSDUCERS/") :transducers)
      (when (string-starts-with? (symbol-name sys) "TRIAL-") :trial)
      (when (string-starts-with? (symbol-name sys) "TRIVIAL-BATTERY/") :trivial-battery)
      sys))
