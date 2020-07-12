{-
# Heading 1

## Heading 2

### Heading 3

#### Heading 4

##### Heading 5

###### Heading 6

Message data type for our _messaging-system_ config.

    It is similar to the `kafka` message dto.
testing again another random text.

**NOTE**: This is really important to know

This allows you to:

* Send messages
* Receive messages, knowing that:
    - nothing works (?)
    - weird huh

As somebody said,

> The best way to code is to not code

Here I show you some haskell code

```haskell
main = putStrLn "dhall rulez"
```
-}
{ remitent: Text                         --| Remitent name
, deliveryStatus: ./DeliveryStatus.dhall --| Status
, body: Text                             --| Message body
}
