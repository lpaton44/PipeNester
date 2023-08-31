namespace App

open Browser.Types
open Thoth.Fetch
open Thoth.Json
open Fable.Core
open Feliz
open Feliz.Router
open Nester
open Nester.Nester
open NesterHelpers


type NewComponents =

    [<ReactComponent>]
    static member OrderForm () =

        let (orderItems, setOrderItems) = React.useState []
        let (editedItems, setEditedItems) = React.useState []
        let (addingItem, setAddingItem) = React.useState false
        let (editingItem, setEditingItem) = React.useState false

        let orderNumberRef = React.useRef None
        let orderRef = React.useRef None

        let (productCode, setProductCode) = React.useState ""
        let (itemQuantity, setItemQuantity) = React.useState ""

        let addNewItemHandler () =
            if (productCode = "") || (itemQuantity = "") then
                ()
            else
                let newItem = (productCode, itemQuantity)
                let newItems =
                    match orderItems with
                    | [] -> [newItem]
                    | _ -> newItem::orderItems

                setProductCode ""
                setItemQuantity ""
                setOrderItems newItems

                setAddingItem false

        let submitHandler () =
            let newOrderNumber = unbox<HTMLInputElement> orderNumberRef.current
            let newOrder = {
                orderNumber = newOrderNumber.value |> int
                order = convertOrderListToString orderItems
            }
            promise {
                let url = "https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/orders.json"
                return! Fetch.post (url, newOrder, caseStrategy = CamelCase)
            }
            |> ignore
            Router.navigate("allOrders")

        Html.div [
            prop.className "w-screen p-5"
            prop.children [
                Html.div [
                    prop.children [
                       Html.div [
                           prop.className "sm:w-full lg:w-3/4 xl:w-1/2 m-auto border border-1 border-solid
                              border-gray-500 rounded-lg p-10"
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
                                          prop.className "border border-1 border-solid border-gray-500
                                             rounded-lg ml-10 w-50 p-2"
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
                                                   prop.className "ml-20 mt-20 text-center text-xl mb-10"
                                                   prop.text "Current Order:"
                                                   ]
                                               if not (orderItems = []) then
                                                   Html.table [
                                                       prop.className "py-2 mt-10 text-center table-auto text-large
                                                          sm:w-3/4 md:w-3/4 lg:w-1/2 xl:w-1/2 m-auto"
                                                       prop.children [
                                                           Html.thead [
                                                           prop.className "border-b font-semibold dark:border-neutral-500"
                                                           prop.children [
                                                               Html.tr [
                                                                   Html.th [ prop.text "Product Code" ]
                                                                   Html.th [ prop.text "Quantity" ]
                                                                   ]
                                                               ]
                                                           ]
                                                           Html.tbody [
                                                              prop.children (
                                                                  match orderItems with
                                                                  | [] -> []
                                                                  | _ ->
                                                                      orderItems
                                                                      |> List.mapi (fun index (productCode, quantity) ->
                                                                         Html.tr [
                                                                             prop.key index
                                                                             prop.className (if index % 2 = 0
                                                                                             then "bg-gray-100"
                                                                                             else "bg-white border-b
                                                                                                dark:border-neutral-500")
                                                                             prop.children [
                                                                                 if editingItem then
                                                                                    Html.td [
                                                                                        Html.input [
                                                                                           prop.id "editProductCode"
                                                                                           prop.name "editProductCode"
                                                                                           prop.type'.text
                                                                                           prop.defaultValue productCode
                                                                                           prop.className "border border-1 border-solid border-gray-500 rounded-lg "
                                                                                           prop.onChange (fun (event : Event) ->
                                                                                                let newProductCode =
                                                                                                    (event.target:?> HTMLInputElement).value
                                                                                                let newOrder = orderItems
                                                                                                                |> List.mapi (fun itemIndex (code, quantity) ->
                                                                                                                    if (itemIndex = index) then
                                                                                                                        (newProductCode, quantity)
                                                                                                                    else
                                                                                                                         (code, quantity))
                                                                                                setEditedItems newOrder
                                                                                           )
                                                                                        ]
                                                                                    ]
                                                                                    Html.td [
                                                                                        Html.input [
                                                                                           prop.id "editQuantity"
                                                                                           prop.name "editQuantity"
                                                                                           prop.type'.number
                                                                                           prop.defaultValue quantity
                                                                                           prop.className "border border-1 border-solid border-gray-500 rounded-lg"
                                                                                           prop.onChange (fun (event : Event) ->
                                                                                                let newQuantity =
                                                                                                    (event.target:?> HTMLInputElement).value
                                                                                                let newOrder = orderItems
                                                                                                                |> List.mapi (fun itemIndex (code, quantity) ->
                                                                                                                    if (itemIndex = index) then
                                                                                                                        (code, newQuantity)
                                                                                                                    else
                                                                                                                         (code, quantity))
                                                                                                setEditedItems newOrder
                                                                                           )
                                                                                        ]
                                                                                    ]
                                                                                    else
                                                                                        Html.td [
                                                                                            prop.text productCode
                                                                                        ]
                                                                                        Html.td [
                                                                                            prop.text quantity
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
                                              ]
                                          ]
                               if  (not (orderItems = []) && not editingItem && not addingItem) then
                                  Html.div [
                                      prop.className "w-full flex justify-items-center"
                                      prop.children [
                                          Html.button [
                                                prop.className "mt-5 m-auto cursor-pointer bg-gray-100 rounded px-2 p-1
                                                  text-large text-black hover:bg-gray-200"
                                                prop.text "Edit Order"
                                                prop.onClick (fun _ -> setEditingItem true)
                                          ]
                                      ]
                                  ]
                               if editingItem then
                                   Html.div [
                                       prop.className "w-full flex justify-items-center"
                                       prop.children [
                                           Html.div [
                                               prop.className "m-auto"
                                               prop.children [
                                               Html.button [
                                                  prop.className "mt-5 mr-5 cursor-pointer bg-green-100 rounded px-2 p-1
                                                    text-large text-black hover:bg-green-200"
                                                  prop.text "Save Edit"
                                                  prop.onClick (fun _ ->
                                                      setOrderItems editedItems
                                                      setEditingItem false)
                                                ]
                                               Html.button [
                                                     prop.className "mt-5  cursor-pointer bg-gray-100 rounded px-2 p-1
                                                       text-large text-black hover:bg-gray-200"
                                                     prop.text "Cancel Edit"
                                                     prop.onClick (fun _ -> setEditingItem false)
                                                   ]
                                               ]
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
                                                     prop.text "Product Code:"
                                                   ]
                                                  Html.input [
                                                     prop.id "code"
                                                     prop.name "code"
                                                     prop.type'.text
                                                     prop.className "ml-6 border border-1 border-solid border-gray-500 rounded-lg p-1"
                                                     prop.onChange (fun (event : Event) ->
                                                         let newProductCode =
                                                             (event.target:?> HTMLInputElement).value
                                                         setProductCode newProductCode
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
                                                  prop.className "ml-16 p-1 border border-1 border-solid border-gray-500 rounded-lg  p-1"
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


                               Html.div [
                                   prop.className "flex justify-end w-full mr-20"
                                   prop.children [
                                       if (not addingItem) && (not editingItem) then
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
                                           if (not addingItem) && (not editingItem) then
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
                let url = "https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/orders.json"
                return! Fetch.get (url, decoder = Decode.dict Order.Decoder, caseStrategy = CamelCase)
           }


        //for array
        (*
        let getDataA (): JS.Promise<Order [] >= // Setup API call inside component to keep all related calls together with the component
            promise {
                let url = "https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/orders.json"
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
                    prop.className "text-center mb-10 text-2xl font-bold"
                    prop.text "All Orders"
                ]
                Html.table [
                    prop.className "sm:w-3/4 xl:w-1/2 m-auto py-2 text-center table-auto text-xl"
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
                           prop.className "py-2 text-center table-auto text-xl scroll-p-0"
                           prop.children (
                               initialOrders
                               |> Map.toList
                               |> List.mapi (fun index (id,orderItem) ->
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
                let url = $"https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/orders/{id}.json"
                return! Fetch.get (url, decoder = Order.Decoder, caseStrategy = CamelCase)
            }


        React.useEffectOnce (fun () ->
            let d = getDataA() // get the data from the API call (Promise)
            d.``then``(fun order -> setOrder (Some order))// On promise return set order state to returned result
            |> ignore // ignore promise after state has been set as React.useEffect always needs to return a unit
        )

        let getNesting pipeL =
            let state = Nesting.createInitialState pipeL
            let nesting = topDown.nestTopDown state
            Results.getGroupedNodes nesting

        let getTreeItems (treeString: string) =
            treeString.Split(";")
               |> Array.toList

        Html.div [
            prop.className "w-screen"
            prop.children [
                Html.h1 [
                    prop.className "text-center mb-10 text-2xl font-bold"
                    prop.text $"Order Number {checkIfFound order}"
                ]
                Html.table [
                    prop.className "py-2 text-center table-auto text-xl sm:w-3/4 md:w-3/4 lg:w-1/2 xl:w-1/2 m-auto"
                    prop.children [
                        Html.thead [
                        prop.className "border-b font-semibold dark:border-neutral-500"
                        prop.children [
                            Html.tr [
                                Html.th [ prop.text "Product Code" ]
                                Html.th [ prop.text "Quantity" ]
                                //Html.th [ prop.text "Diameter" ]
                                //Html.th [ prop.text "Socket" ]
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
                    prop.className "w-1/3 m-auto mb-"
                    prop.children [
                         Html.h1 [
                                prop.className "text-center text-xl font-bold mt-20 mb-10"
                                prop.text $"Nesting"
                            ]
                         Html.ul [
                            prop.children (getNesting (convertOrderToPipeList order)
                                |> List.mapi(fun index tree ->
                                    Html.li [
                                        prop.className
                                            (if index % 2 = 0 then "bg-gray-100 p-2 text-center"
                                                else " text-center bg-white border-b dark:border-neutral-500 p-2")
                                        prop.children (getTreeItems tree
                                        |> List.map (fun treeItem ->
                                            if treeItem.Contains("tree") then
                                                Html.h1 [
                                                    prop.className "text-xl font-semibold"
                                                    prop.text $"{treeItem}"
                                                ]
                                            else
                                                let itemWithIndent =
                                                   treeItem.Split(',')
                                                   |> Array.map (fun i -> i.Trim())
                                                   |> Array.toList
                                                   |> List.map int


                                                let indentValue = itemWithIndent[0] * 20
                                                let indent =  $"{indentValue}px"

                                                Html.p [
                                                    prop.style [
                                                        style.textIndent indent
                                                    ]
                                                    prop.text $"{itemWithIndent[1]}"
                                                ]
                                            )
                                        )
                                    ]
                                )
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
