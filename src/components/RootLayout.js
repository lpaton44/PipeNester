import { Outlet } from 'react-router-dom';
import Header from './UI/Header';

function RootLayout() {
  return (
    <>
      <Header /> 
      <main>
        <Outlet />
      </main>
    </>
  );
}

export default RootLayout;