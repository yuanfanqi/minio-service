package com.minioservice.service.impl;

import com.minioservice.dao.LogMinioOperateDao;
import com.minioservice.entity.LogMinioOperate;
import com.minioservice.service.LogMinioOperateService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * minio文件操作记录(LogMinioOperate)表服务实现类
 *
 * @author makejava
 * @since 2023-02-08 11:43:06
 */
@Service("logMinioOperateService")
public class LogMinioOperateServiceImpl implements LogMinioOperateService {
    @Resource
    private LogMinioOperateDao logMinioOperateDao;

    /**
     * 通过ID查询单条数据
     *
     * @param id 主键
     * @return 实例对象
     */
    @Override
    public LogMinioOperate queryById(Long id) {
        return this.logMinioOperateDao.queryById(id);
    }

    /**
     * 分页查询
     *
     * @param logMinioOperate 筛选条件
     * @param pageRequest     分页对象
     * @return 查询结果
     */
    @Override
    public Page<LogMinioOperate> queryByPage(LogMinioOperate logMinioOperate, PageRequest pageRequest) {
        long total = this.logMinioOperateDao.count(logMinioOperate);
        return new PageImpl<>(this.logMinioOperateDao.queryAllByLimit(logMinioOperate, pageRequest), pageRequest, total);
    }

    /**
     * 新增数据
     *
     * @param logMinioOperate 实例对象
     * @return 实例对象
     */
    @Override
    public LogMinioOperate insert(LogMinioOperate logMinioOperate) {
        this.logMinioOperateDao.insert(logMinioOperate);
        return logMinioOperate;
    }

    /**
     * 修改数据
     *
     * @param logMinioOperate 实例对象
     * @return 实例对象
     */
    @Override
    public LogMinioOperate update(LogMinioOperate logMinioOperate) {
        this.logMinioOperateDao.update(logMinioOperate);
        return this.queryById(logMinioOperate.getId());
    }

    /**
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    @Override
    public boolean deleteById(Long id) {
        return this.logMinioOperateDao.deleteById(id) > 0;
    }

    /**
     * @Description: 查询单个文件是否存在：文件名or文件hash是否相同
     * @param: [fileName, fileHash, sourceService]
     * @return: LogMinioOperate
     * @Author: song
     * @Date: 2023/2/9 9:59
     */
    @Override
    public List<LogMinioOperate> isFileExist(String fileName, String fileHash, String sourceService) {
        return logMinioOperateDao.isFileExist(fileName, fileHash, sourceService);
    }

}
