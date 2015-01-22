;; MusicBrainz resources.

(module resource mzscheme
  (provide (all-defined))

  ;; All MusicBrainz objects (artists, releases, tracks) are modeled
  ;; as resources.  A resource has a unique ID, a 36-character string
  ;; of letters, numbers, and hyphens.
  (define-struct resource (id))

  ;; An artist has a name string and a sortname string (indicating how
  ;; the name should be sorted, e.g. "David Bowie" -> "Bowie, David").
  ;; An artist also has a disambiguation string, a (natural language)
  ;; description used to disambiguiate the artist from others with the
  ;; same name.
  (define-struct (artist resource) (name sortname disambiguation))

  ;; A release has a title string, a type symbol (album, compilation,
  ;; single, etc), and a status symbol (official, promotion, bootleg).
  (define-struct (release resource) (title type status))

)
