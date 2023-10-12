import {React, useState} from 'react';
import {ConfigProvider, Layout, theme} from 'antd';
import {Divider} from 'antd';
import QuickAccess from './QuickAccess'
import ToolBar from './ToolBar'
import PaperList from './PaperList'
import SearchBar from './SearchBar'
import Paper from './Paper';
import {Content} from 'antd/es/layout/layout';

const {Header, Sider} = Layout;

const paperSample = {
    title: 'Paper Title'
}

const App = () => {
    const {token: {colorBgContainer}, } = theme.useToken();
    const [isFavorite, setIsFavorite] = useState([]);
    return (
        <ConfigProvider theme={{algorithm: [theme.defaultAlgorithm, theme.compactAlgorithm]}}>
            <Layout
                style={{height: '100vh', overflow: 'hidden'}}
            >
                <Sider style={{background: colorBgContainer}}>
                    <QuickAccess />
                </Sider>
                <Layout style={{background: colorBgContainer}}>
                    <Header style={{background: colorBgContainer, width: '100%', whiteSpace:'nowrap'}}>
                        <ToolBar isFavorite={isFavorite} />
                        <Divider type='vertical' style={{padding: '0em 1em'}} />
                        <SearchBar />
                    </Header>
                    <Divider type='horizontal' style={{margin: '0em'}} />
                    <Content style={{background: colorBgContainer}}>
                        <Layout style={{background: colorBgContainer, height: '100%'}}>
                            <Sider width={'30%'} style={{background: colorBgContainer}}>
                                <PaperList />
                            </Sider>
                            <Content style={{background: colorBgContainer}}>
                                <Paper paper={paperSample} />
                            </Content>
                        </Layout>
                    </Content>
                </Layout>
            </Layout>
        </ConfigProvider>
    );
};
export default App;
