context("tp")

library(magrittr)

test_that("example 1", {
  c1 <- pc_chord(c(8, 6, 9, 10))
  c2 <- tp(c1, 4)
  expect_equal(
    get_bass_pc(c2), 0
  )
  expect_equal(
    setdiff(as.numeric(pc_set(c2)),
            get_bass_pc(c2)),
    c(1, 2, 10)
  )
})

test_that("reversible", {
  n <- 10
  for (i in seq_len(10)) {
    c1 <- pc_chord(sample(11, 5))
    int <- sample(-11:11, 1)
    c2 <- tp(c1, int)
    c3 <- tp(c2, -int)
    expect_equal(c1, c3)
  }
})

test_that("pc_set", {
  p1 <- pc_set(c(0, 4, 7))
  expect_is(tp(p1, 2), "pc_set")
  expect_equal(
    tp(p1, 2) %>% as.integer,
    c(2, 6, 9)
  )
  expect_equal(
    c(0, 4, 7) %>% pc_set %>% tp(-2) %>% as.integer,
    c(2, 5, 10)
  )
})

test_that("corpora", {
  corpora <- list()

  corpora$pc_set <-
    list(
      list(
        pc_set(c(0, 6, 9)),
        pc_set(c(5, 6, 11)),
        pc_set(c(2, 3, 4))
      ) %>% vec("pc_set"),
      list(
        pc_set(c(0, 7, 9)),
        pc_set(c(4, 6, 11)),
        pc_set(c(1, 3, 4))
      ) %>% vec("pc_set")
    ) %>% corpus("pc_set")

  corpora$pc_chord <- list(
    list(
      pc_chord(c(7, 6, 9)),
      pc_chord(c(5, 1, 11)),
      pc_chord(c(2, 3, 4))
    ) %>% vec("pc_chord"),
    list(
      pc_chord(c(2, 6, 9)),
      pc_chord(c(5, 2, 11)),
      pc_chord(c(2, 3, 6))
    ) %>% vec("pc_chord")
  ) %>% corpus("pc_chord")

  corpora$pi_chord <- list(
    list(
      pi_chord(c(60, 64, 67)),
      pi_chord(c(52, 78, 99)),
      pi_chord(c(40, 41, 50))
    ) %>% vec("pi_chord"),
    list(
      pi_chord(c(61, 64, 67)),
      pi_chord(c(52, 79, 99)),
      pi_chord(c(39, 41, 50))
    ) %>% vec("pi_chord")
  ) %>% corpus("pi_chord")


  for (encoded in c(TRUE, FALSE)) {
    for (representation in c("pc_set", "pc_chord", "pi_chord")) {
      if (representation == "pi_chord") {
        next
      }

      interval <- 3

      corpus <- corpora[[representation]]

      v1 <- purrr::map(corpus, function(sequence) {
        purrr::map(sequence, function(chord) {
          hrep::tp(chord, interval)
        }) %>% vec(representation)
      }) %>% corpus(representation)

      if (encoded) {
        v1 <- encode(v1)
      }

      v2 <- if (encoded) {
        corpus %>% encode() %>% tp(interval)
      } else {
        corpus %>% tp(interval)
      }

      expect_equal(v1, v2)
    }
  }
})
