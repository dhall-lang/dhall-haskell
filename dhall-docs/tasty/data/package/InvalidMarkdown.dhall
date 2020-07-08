-- Represents an status in our messaging system `
-- look, another line

let Pair = ./Pair.dhall

let DeliveryStatus =
    < Sent: Pair Natural Text {-| Message has been _sent_. It carries a ./Pair.dhall reference
                                    containing on the `first` field number of retries
                                    and on the `second` field the delivered text
                                -}
    | InProgress                -- | Message is on the way!
    | InQueue                   -- | Delivery for this message **hasn't** started
    >

in DeliveryStatus
