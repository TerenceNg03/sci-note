import {React, useState, useEffect} from 'react';
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

const onresize = () => {
    window.onresize = fixSearchbar;
    setTimeout(fixSearchbar, 10)
    setTimeout(fixSearchbar, 100)
    setTimeout(fixSearchbar, 1000)
};

const fixSearchbar = () => {
    document.getElementById("searchbar").parentElement.style.width =
        document.getElementById("header").clientWidth -
        document.getElementById("toolbar").clientWidth - 10 + 'px';
}

const App = () => {
    const {token: {colorBgContainer}, } = theme.useToken();
    const [isFavorite, setIsFavorite] = useState([]);
    useEffect(fixSearchbar);
    useEffect(onresize);

    return (
        <ConfigProvider theme={{algorithm: [theme.defaultAlgorithm, theme.compactAlgorithm]}}>
            <Layout
                style={{height: '100vh', overflow: 'hidden'}}
            >
                <Sider width={'15em'} style={{background: colorBgContainer}}>
                    <QuickAccess />
                </Sider>
                <Layout style={{background: colorBgContainer}}>
                    <Header id="header" style={{background: colorBgContainer, padding: '0em'}}>
                        <ToolBar isFavorite={isFavorite} />
                        <SearchBar />
                    </Header>
                    <Divider type='horizontal' style={{display: 'inline-flex', margin: '0em'}} />
                    <Content style={{background: colorBgContainer}}>
                        <Layout style={{background: colorBgContainer, height: '100%'}}>
                            <Sider width={'24em'} style={{background: colorBgContainer}}>
                                <PaperList />
                            </Sider>
                            <Content style={{background: colorBgContainer}}>
                                <Layout style={{background: colorBgContainer, height: '100%'}}>
                                    <Sider width={'1em'} style={{background: colorBgContainer, height: '100%'}}>
                                        <Divider type='vertical' style={{height: '100%'}} />
                                    </Sider>
                                    <Content style={{background: colorBgContainer}}>
                                        <Paper paper={paperSample} />
                                    </Content>
                                </Layout>
                            </Content>
                        </Layout>
                    </Content>
                </Layout>
            </Layout>
        </ConfigProvider>
    );
};
export default App;
