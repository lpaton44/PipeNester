namespace App

open Browser.Types
open Thoth.Fetch
open Thoth.Json
open Fable.Core
open Feliz
open Feliz.Router
open Fable.FontAwesome.Free
open Fable.FontAwesome

type Order =
        {
          orderNumber: int
          order: string
        }
        static member Decoder =
            Decode.object (fun get ->
                  {
                    orderNumber = get.Required.Field "orderNumber" Decode.int
                    order = get.Required.Field "order" Decode.string
                  }
             )

        static member Encoder (order : Order) =
            Encode.object [
                "orderNumber", Encode.int order.orderNumber
                "order", Encode.string order.order
            ]

type Components =

    [<ReactComponent>]
    static member Header() =
        Html.header [
            prop.className "h-30 bg-teal-600 text-white "
            prop.children [
                Html.div [
                    prop.className "flex items-center"
                    prop.children [
                       Html.div [
                           prop.className "color-white mt-5 mr-2  ml-10"
                           prop.children [
                              Fa.i [
                                  Fa.Size Fa.ISize.Fa2x
                                  Fa.Solid.Truck ] []
                           ]
                       ]

                       Html.h1 [
                           prop.className "mt-5 text-3xl font-semibold"
                           prop.text "PipeNester"
                       ]
                    ]
                ]

                Html.div [
                    prop.className "items-bottom justify-end flex mr-10 mb-5"
                    prop.children[
                        Html.nav [
                            prop.className "mr-10 p-5 link text-white font-semibold text-xl"
                            prop.children [
                                Html.a [
                                    prop.onClick (fun _ -> Router.navigate "allOrders")
                                    prop.className "mr-10 hover:text-gray-500 cursor-pointer"
                                    prop.text "All Orders"
                                ]
                                Html.a [
                                    prop.onClick (fun _ -> Router.navigate "newOrder")
                                    prop.className "hover:text-gray-500 cursor-pointer"
                                    prop.text "Add Order"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


    [<ReactComponent>]
    static member OrderForm () =

        let (orderItems, setOrderItems) = React.useState []
        let (addingItem, setAddingItem) = React.useState false
        let (editingItem, setEditingItem) = React.useState false

        let orderNumberRef = React.useRef None

        let (itemDiameter, setItemDiameter) = React.useState ""
        let (itemQuantity, setItemQuantity) = React.useState ""

        let addNewItemHandler () =
            if (itemDiameter = "") || (itemQuantity = "") then
                ()
            else

                let newItem = $"({itemDiameter}, {itemQuantity})"
                let newItems = newItem::orderItems

                setItemDiameter ""
                setItemQuantity ""
                setOrderItems newItems
                setAddingItem false

        let submitHandler () =
            let newOrderNumber = unbox<HTMLInputElement> orderNumberRef.current
            let newOrder = {
                orderNumber = newOrderNumber.value |> int
                order = orderItems |> String.concat "; "
            }
            promise {
                let url = "https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes.json"
                return! Fetch.post (url, newOrder, caseStrategy = CamelCase)
            }
            |> ignore
            Router.navigate("allOrders")

        let orderString = orderItems |> String.concat "; "

        Html.div [
            prop.className "w-screen p-5"
            prop.children [
                Html.div [
                    prop.children [
                       Html.div [
                           prop.className "sm:w-full lg:w-3/4 xl:w-1/2 m-auto border border-1 border-solid border-gray-500 rounded-lg p-10"
                           prop.children [
                               Html.div [
                                   prop.className "mx-5 mb-5"
                                   prop.children [
                                       Html.label [
                                            prop.className "text-left text-3xl p-5 font-semibold"
                                            prop.text "Order Number:"
                                       ]
                                   ]
                               ]
                               Html.div [
                                   prop.children [
                                       Html.input [
                                          prop.id "orderNumber"
                                          prop.name "orderNumber"
                                          prop.placeholder "Enter a number"
                                          prop.type'.number
                                          prop.className "border border-1 border-solid border-gray-500 rounded-lg ml-10 w-50 p-2"
                                          prop.ref orderNumberRef
                                      ]
                                   ]
                               ]
                               Html.div [
                                   prop.className "mt-10 mx-5"
                                   prop.children [
                                       Html.label [
                                            prop.className "text-left text-3xl p-5 font-semibold"
                                            prop.text "Order Details:"
                                       ]
                                   ]
                               ]
                               Html.div [
                                   prop.className "mt-5"
                                   prop.children [
                                       Html.div [
                                           prop.children [
                                               Html.label [
                                                   prop.className "ml-20 mt-5 text-xl"
                                                   prop.text $"Current Order: {orderString}"

                                                   ]
                                               ]
                                           ]

                                       Html.div [
                                           prop.className "mt-7 ml-10"
                                           prop.children [
                                               if (addingItem) then
                                                   Html.div [
                                                       prop.className "ml-5 mb-3"
                                                       prop.children [
                                                           Html.label [
                                                              prop.className "p-5 ml-1 text-xl"
                                                              prop.text "Diameter:"
                                                            ]
                                                           Html.input [
                                                              prop.id "diameter"
                                                              prop.name "diameter"
                                                              prop.type'.number

                                                              prop.className "border border-1 border-solid border-gray-500 rounded-lg ml-10 p-1"
                                                              prop.onChange (fun (event : Event) ->
                                                                  let newDiameter =
                                                                      (event.target:?> HTMLInputElement).value
                                                                  setItemDiameter newDiameter
                                                              )
                                                           ]
                                                       ]
                                                   ]

                                                   Html.div [
                                                       prop.className "ml-5 p-2 mb-10"
                                                       prop.children [
                                                          Html.label [
                                                              prop.className "p-5 text-xl"
                                                              prop.text "Quantity:"
                                                          ]
                                                          Html.input [
                                                            prop.id "quantity"
                                                            prop.name "quantity"
                                                            prop.type'.number
                                                            prop.className "ml-10 p-1 border border-1 border-solid border-gray-500 rounded-lg ml-10 p-1"
                                                            prop.onChange (fun (event : Event) ->
                                                                  let newQuantity =
                                                                      (event.target:?> HTMLInputElement).value
                                                                  setItemQuantity newQuantity
                                                              )
                                                         ]
                                                       ]
                                                   ]
                                               ]
                                           ]
                                       ]
                               ]
                               Html.div [
                                   prop.className "flex justify-end w-full mr-20"
                                   prop.children [
                                       if (not addingItem) then
                                           Html.button [
                                              prop.className "cursor-pointer bg-teal-500 rounded p-3
                                                text-xl text-black hover:bg-teal-600"
                                              prop.text "Add Item to Order"
                                              prop.onClick (fun _ -> setAddingItem true)
                                           ]
                                   ]
                               ]
                               Html.div [
                                   prop.className "flex w-full items-center justify-end mr-20 gap-5 mt-10 mr-10"
                                   prop.children [
                                       if addingItem then
                                           Html.button [
                                              prop.className "cursor-pointer bg-green-500 rounded p-3 px-4
                                               text-xl text-black hover:bg-green-600"
                                              prop.text "Add Item"
                                              prop.onClick (fun _ -> addNewItemHandler())
                                           ]

                                           Html.button [
                                              prop.className "cursor-pointer bg-gray-300 rounded p-3
                                                  px-6 text-xl text-black hover:bg-red-300"
                                              prop.text "Close"
                                              prop.onClick (fun _ -> setAddingItem false)

                                           ]

                                       else
                                           Html.button [
                                              prop.className "cursor-pointer bg-green-500 rounded p-3 text-xl
                                                text-black hover:bg-green-600"
                                              prop.text "Save Order"
                                              prop.onClick (fun _ -> submitHandler())
                                           ]
                                           Html.button [
                                              prop.className "cursor-pointer bg-gray-300 rounded p-3
                                                  text-xl text-black hover:bg-red-300"
                                              prop.text "Cancel Order"
                                              prop.onClick (fun _ -> Router.navigate("/allOrders"))
                                           ]
                                   ]
                               ]
                           ]
                       ]
                    ]
                ]
            ]
        ]


    [<ReactComponent>]
    static member OrderList () =

        let (initialOrders, setInitialOrders) = React.useState Map.empty //Array.empty

        //for dict
        let getDataA () = // Setup API call inside component to keep all related calls together with the component
            promise {
                let url = "https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes.json"
                return! Fetch.get (url, decoder = Decode.dict Order.Decoder , caseStrategy = CamelCase)
           }


        //for array
        (*let getDataA (): JS.Promise<Order [] >= // Setup API call inside component to keep all related calls together with the component
            promise {
                let url = "https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes.json"
                return! Fetch.get (url, caseStrategy = CamelCase)
            }*)


        React.useEffectOnce (fun () ->
            let d = getDataA() // get the data from the API call (Promise)
            d.``then``setInitialOrders// On promise return set order state to returned result
            |> ignore // ignore promise after state has been set as React.useEffect always needs to return a unit
        )

        let getListItems (orderString : string) =
            let removeSquareBrackets = orderString.Trim('[', ']').Trim()
            let splitItems = removeSquareBrackets.Split(';')
                             |> Array.toList
                             //|> List.map (fun i -> i.Trim('(', ')'))

            Html.ul [
                prop.children (splitItems
                |> List.map(fun item ->
                    Html.li [
                        prop.text $"{item}"
                        prop.onClick (fun _ -> Router.navigate(""))
                    ]
                ))
            ]

        Html.div [
            prop.className "px-20 w-screen"
            prop.children [
                Html.h1 [
                    prop.className "text-center mb-10 text-xl underline font-bold"
                    prop.text "All Orders"
                ]
                Html.table [
                    prop.className "sm:w-full lg:w-3/4 xl:w-1/2 m-auto py-2 text-center table-auto text-xl"
                    prop.children [
                        Html.thead [
                        prop.className "border-b font-semibold dark:border-neutral-500"
                        prop.children [
                            Html.tr [
                                Html.th [ prop.text "Order Number" ]
                                Html.th [ prop.text "Order Summary" ]
                                ]
                            ]
                        ]
                        Html.tbody [
                           prop.className "py-2 text-center table-auto text-xl"
                           prop.children (
                               initialOrders
                               |> Map.toList
                               |> List.mapi (fun index (id, orderItem) ->
                                   Html.tr [
                                       prop.key orderItem.orderNumber
                                       prop.onClick (fun _ -> Router.navigate($"/allOrders/{id}"))
                                       prop.className (
                                          if index % 2 = 0 then "bg-gray-100"
                                          else "bg-white border-b dark:border-neutral-500")
                                       prop.children [
                                           Html.td [
                                               prop.text $"{orderItem.orderNumber}"
                                           ]
                                           Html.td [
                                               prop.children (getListItems orderItem.order)
                                           ]
                                       ]
                                   ]
                               )
                           )
                       ]
                    ]
                ]
                Html.div [
                    prop.className "w-full  grid justify-items-center"
                    prop.children [
                         Html.button [
                             prop.className "cursor-pointer bg-gray-300
                                rounded p-3 px-5 text-xl text-black hover:bg-green-300 mt-10"
                             prop.text "Add new Order"
                             prop.onClick (fun _ -> Router.navigate("/newOrder"))
                         ]
                    ]
                ]
            ]
        ]

    [<ReactComponent>]
    static member OrderDetails (id : string) =

        let (order, setOrder) = React.useState None
        let getDataA (): JS.Promise<Order>= // Setup API call inside component to keep all related calls together with the component
            promise {
                let url = $"https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes/{id}.json"
                return! Fetch.get (url, decoder = Order.Decoder, caseStrategy = CamelCase)
            }


        React.useEffectOnce (fun () ->
            let d = getDataA() // get the data from the API call (Promise)
            d.``then``(fun order -> setOrder (Some order))// On promise return set order state to returned result
            |> ignore // ignore promise after state has been set as React.useEffect always needs to return a unit
        )

        let removeBracketsFromPair (pair: string List) =
             pair
            |> List.map (fun i ->
                i.Trim([|'('; ')'|]))


        let getDetails (orderString : string) =
            let orderItems =
                orderString.Trim([|'['; ']'; ' '|]).Split(';')
                |> Array.toList

            let itemDetails =
                orderItems
                    |> List.map (fun item -> item.Trim().Split(',') |> Array.toList)
                    |> List.map removeBracketsFromPair
            itemDetails

        let checkIfFound =
            match order with
            | Some value -> $"{value.orderNumber}"
            | None -> "No order found."

        Html.div [
            prop.className "w-screen"
            prop.children [
                Html.h1 [
                    prop.className "text-center mb-10 text-xl font-bold"
                    prop.text $"Order Number {checkIfFound}"
                ]
                Html.table [
                    prop.className "py-2 text-center table-auto text-xl sm:w-full lg:w-3/4 xl:w-1/2 m-auto"
                    prop.children [
                        Html.thead [
                        prop.className "border-b font-semibold dark:border-neutral-500"
                        prop.children [
                            Html.tr [
                                Html.th [ prop.text "Diameter" ]
                                Html.th [ prop.text "Quantity" ]
                                ]
                            ]
                        ]
                        Html.tbody [
                                prop.children (
                                    match order with
                                    | Some nonEmptyOrder ->
                                        getDetails(nonEmptyOrder.order)
                                        |> List.mapi (fun index details ->
                                               Html.tr [
                                                   prop.key details[0]
                                                   prop.className (if index % 2 = 0 then "bg-gray-100" else "bg-white border-b dark:border-neutral-500")
                                                   prop.children [
                                                       Html.td [
                                                           prop.text $"{details[0]}"
                                                       ]
                                                       Html.td [
                                                           prop.text $"{details[1]}"

                                                       ]
                                                   ]
                                               ]
                                            )
                                    | _ -> []
                                )
                        ]
                    ]
                ]
                Html.div [
                    prop.className "w-full grid justify-items-center"
                    prop.children [
                         Html.button [
                             prop.className "cursor-pointer bg-gray-300 rounded p-3 px-5  text-xl text-black hover:bg-red-300 mt-10"
                             prop.text "Back"
                             prop.onClick (fun _ -> Router.navigate("/allOrders"))
                         ]
                    ]
                ]
            ]
        ]

    /// <summary>
    /// A React component that uses Feliz.Router
    /// to determine what to show based on the current URL
    /// </summary>
    [<ReactComponent>]
    static member Router() =
        let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
        React.router [
            router.onUrlChanged updateUrl
            router.children [
                Components.Header()

                match currentUrl with
                | [ "newOrder" ] -> Components.OrderForm()
                | ["allOrders"] -> Components.OrderList()
                | ["allOrders"; id] -> Components.OrderDetails(id)
                | otherwise -> Html.h1 "Not found"
            ]
        ]
