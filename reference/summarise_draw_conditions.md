# Summarise Conditions Raised by Forecast Draws

Groups condition messages by their first line and counts how often each
occurred, so that a condition raised in many draws is reported once.

## Usage

``` r
summarise_draw_conditions(messages, n_draws)
```

## Arguments

- messages:

  Character vector of condition messages.

- n_draws:

  Total number of draws.

## Value

A named character vector of cli bullets.
