import './styles/App.css';
import { RouterProvider, createBrowserRouter } from 'react-router-dom';
import Order, {loader as OrderLoader} from './components/Order'
import RootLayout from './components/RootLayout';
import Orders, {loader as OrdersLoader} from './components/Orders';
import NewOrder from './components/NewOrder';

function App() {
  const router = createBrowserRouter([
      {
        path: '/',
        element: <RootLayout />,
        children: [
        {
          path: '/orders',
          element: <Orders />,
          loader: OrdersLoader,
        }, 
        {
          path: '/orders/:id',
          element: <Order />,
          loader: OrderLoader, 
        }, 
        {
          path: '/new', element: <NewOrder />
        }, ]
      }
  ])

  return (
      <RouterProvider router={router}/>
  );
}

export default App;
