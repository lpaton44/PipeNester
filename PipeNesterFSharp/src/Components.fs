namespace App

open Feliz
open Feliz.Router
open Fable.FontAwesome

type Order = {
        order: (string*string) List
        orderNumber: int
        id: Option<string>
    }

type Components =

    [<ReactComponent>]
    static member Header() =
        Html.header [
            prop.className "h-20 bg-teal-600 text-white "
            prop.children [
                Html.div [
                    prop.className "flex items-center"
                    prop.children [
                       Html.i [
                           prop.className "fa-regular fa-truck fa-xs"
                       ]

                       Html.h1 [
                           prop.className "mt-5 ml-10 text-2xl font-bold"
                           prop.text "PipeNester"
                          ]
                    ]
                ]
                Html.div [
                    prop.className "items-bottom justify-end flex mr-10"
                    prop.children[
                        Html.nav [
                            prop.className "mr-6 link text-white text-lg font-semibold hover:text-gray-500"
                            prop.text "All Orders"
                        ]
                        Html.nav [
                            prop.className "mr-6 link text-white text-lg font-semibold hover:text-gray-500"
                            prop.text "New Order"
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
            prop.children [
                Html.form [
                    prop.children [
                        Html.p [
                            prop.children [
                                Html.label [
                                    prop.className "mt-20"
                                    prop.text "Order Number:"
                                ]
                                Html.input [
                                    prop.id "orderNumber"
                                    prop.name "orderNumber"
                                    prop.defaultValue ""
                                    prop.type'.text

                                ]
                            ]
                        ]

                        Html.p [
                            prop.children [
                                Html.label [
                                    prop.text "Order Details:"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


    (*[<ReactComponent>]
    static member OrderList() =
        let orders = [

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
                            Html.tbody [
                                prop.children (
                                    orders
                                    |> List.mapi (fun index orderItem ->
                                        Html.tr [
                                            prop.key orderItem.id
                                            prop.className (if index % 2 = 0 then "bg-gray-100" else "bg-white border-b dark:border-neutral-500")
                                            prop.children [

                                            ]
                                        ]

                                    ))
                            ]
                        ]
                ]
            ]

        ]
    ]
    ]
*)




    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member HelloWorld() = Html.h1 "Hello World"

    /// <summary>
    /// A stateful React component that maintains a counter
    /// </summary>
    [<ReactComponent>]
    static member Counter() =
        let (count, setCount) = React.useState(0)
        Html.div [
            Html.h1 count
            Html.button [
                prop.className "bg-orange-500 rounded-full p-2"
                prop.onClick (fun _ -> setCount(count + 1))
                prop.text "Increment"
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
                | otherwise -> Html.h1 "Not found"
            ]
        ]
