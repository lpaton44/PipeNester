namespace App

open Thoth.Fetch
open Thoth.Json
open Feliz
open Feliz.Router
open Fable.FontAwesome.Free
open Fable.FontAwesome

type Order =
        {
          id: int
          orderNumber: int
          order: string
        }

        static member Decoder =
            Decode.object (fun get ->
                  {
                    id = get.Required.Field "id" Decode.int
                    orderNumber =  get.Required.Field "orderNumber" Decode.int
                    order = get.Required.Field "order" Decode.string
                  }
             )


        static member Encoder (order : Order) =
            Encode.object [
                "id", Encode.int order.id
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
                                    prop.className "mr-10 hover:text-gray-500"
                                    prop.text "All Orders"
                                ]
                                Html.a [
                                    prop.onClick (fun _ -> Router.navigate "newOrder")
                                    prop.className "hover:text-gray-500"
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
        let (orderItems, setOrderItems) = React.useState([])
        let (addingItem, setAddingItem) = React.useState(false)
        let (editingItem, setEditingItem) = React.useState(false)

        let mutable itemDiameterRef = React.useRef("")
        let mutable itemQuantityRef = React.useRef("")
        let mutable orderNumberRef = React.useRef(None)
        let mutable orderRef = React.useRef([])

        let addNewItemHandler () =
            if (itemDiameterRef.current = "") || (itemQuantityRef.current = "") then
                ()
            else
                let newItem = [itemDiameterRef.current, itemQuantityRef.current]
                let newItems = newItem@orderItems

                orderRef.current <-  newItems
                itemDiameterRef.current <- ""
                itemQuantityRef.current <- ""
                setOrderItems newItems
                setAddingItem false

        Html.div [
            prop.className "flex justify-items-start "

            prop.children [
                Html.form [
                    prop.className "ml-10 w-full"

                    prop.children [
                       Html.p [
                           prop.className "ml-10 w-full"
                           (*prop.style [
                               style.borderColor color.red
                               style.borderWidth 1
                           ]*)
                           prop.children [
                               Html.div [
                                   prop.className "flex items-start w-full"
                                   prop.children [
                                       Html.label [
                                            prop.className "text-left text-2xl p-5 font-semibold "
                                            prop.text "Order Number:"
                                       ]
                                   ]
                               ]
                               Html.div [
                                   prop.className "flex items-start"
                                   prop.children [
                                       Html.input [
                                          prop.id "orderNumber"
                                          prop.name "orderNumber"
                                          prop.defaultValue ""
                                          prop.type'.number
                                          prop.style [
                                              style.borderRadius 10
                                              style.borderColor color.gray
                                              style.borderWidth 1
                                          ]
                                          prop.className "ml-3 w-7/10"
                                          prop.ref orderNumberRef
                                      ]
                                   ]
                               ]
                               Html.div [
                                   prop.className "flex items-start"
                                   prop.children [
                                       Html.label [
                                            prop.className " p-5 text-2xl p-5 font-semibold"
                                            prop.text $"Order Details:"
                                        ]
                                   ]
                               ]
                               Html.div [
                                   prop.className "flex items-start"
                                   prop.children [
                                       Html.input [
                                            prop.id "order"
                                            prop.name "order"
                                            prop.defaultValue ""
                                            prop.type'.text
                                            prop.style [
                                                style.borderRadius 10
                                                style.borderColor color.gray
                                                style.borderWidth 1
                                            ]
                                            prop.className " mb-10 w-7/10 ml-2"
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
    static member OrderList() =

        let orders =
                promise {
                    let url = "https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes.json"
                    return! Fetch.get(url, decoder = Decode.list Order.Decoder)
                }

        let orders =
            [
                {
                    id =  1
                    orderNumber = 1
                    order = "[(3,25), (2, 75), (5,110)]"
                };
                {
                    id =  2
                    orderNumber = 2
                    order = "[(5,25), (1, 75), (10,110)]"
                }
            ]

        let listItems =
            orders |> List.map
                          (fun item ->
                             let order = item.order
                             Html.li [
                                 prop.text $"{order}"
                             ])
        Html.div [
            prop.children [
                Html.h1 [
                    prop.className "text-xl font-bold"
                ]
                Html.table [
                    prop.className "min-w-full py-2 text-center table-auto text-xl"
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
                           prop.children (
                               orders
                               |> List.mapi (fun index orderItem ->
                                   Html.tr [
                                       prop.key orderItem.id
                                       prop.className (if index % 2 = 0 then "bg-gray-100" else "bg-white border-b dark:border-neutral-500")
                                       prop.children [
                                           Html.td [

                                               prop.text $"{orderItem.orderNumber}"
                                           ]
                                           Html.td [
                                               prop.children [
                                                   Html.ul [
                                                       prop.text $"{orderItem.order}"
                                                   ]
                                               ]
                                           ]
                                       ]
                                   ]
                               )
                           )
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
                | otherwise -> Html.h1 "Not found"
            ]
        ]
