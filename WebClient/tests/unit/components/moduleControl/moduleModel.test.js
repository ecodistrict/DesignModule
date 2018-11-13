'use strict';

import ModuleModel, { validateStatus, ModuleStatus } from 'components/moduleControl/moduleModel';

describe('ModuleModel', () => {
    
    it('initializes from the constructor', () => {
        const id = 'Module1';
        const name = 'Super Module';
        const progress = 12345;
        const status = 'calc';

        const moduleModel = new ModuleModel({
            id,
            name,
            progress,
            status
        });

        expect(moduleModel.id).toEqual(id);
        expect(moduleModel.name).toEqual(name);
        expect(moduleModel.progress).toEqual(progress);
        expect(moduleModel.status).toEqual(status);
    });

    it('status validation', () => {
        expect(validateStatus(ModuleStatus.IDLE)).toBe(true);
        expect(validateStatus(ModuleStatus.LOCKED)).toBe(true);
        expect(validateStatus(ModuleStatus.BUSY)).toBe(true);
        expect(validateStatus(ModuleStatus.READY)).toBe(true);
        expect(validateStatus(ModuleStatus.CALCULATING)).toBe(true);
        expect(validateStatus(ModuleStatus.REMOVED)).toBe(true);

        expect(validateStatus('something else')).toBe(false);
    });

    it('handles incorrect status during construction', () => {
        const status = 'wrong status';

        expect(() => new ModuleModel({ status })).toThrow(); 
    });

    it('resets the properties with set() call', () => {
        const moduleModel = new ModuleModel({
            id: '1',
            name: '2',
            progress: 123,
            status: 'ready'
        });

        const name = 'Super Module';
        const progress = 12345;
        const status = 'calc';

        moduleModel.set({
            name,
            progress,
            status
        });

        expect(moduleModel.name).toEqual(name);
        expect(moduleModel.progress).toEqual(progress);
        expect(moduleModel.status).toEqual(status);
    });

    it('handles incorrect status change', () => {
        const moduleModel = new ModuleModel({
            id: '1',
            name: '2',
            progress: 123,
            status: 'ready'
        });

        const status = 'wrong status';

        expect(() => moduleModel.set({ status })).toThrow();
        expect(() => moduleModel.status = status).toThrow();
    });

    it('change events are sent only once when something changed', () => {
        const moduleModel = new ModuleModel({
            id: '1',
            name: '2',
            progress: 123,
            status: 'ready'
        });

        const onChangeCallback = jest.fn();
        const onNameCallback = jest.fn();
        const onProgressCallback = jest.fn();
        const onStatusCallback = jest.fn();

        moduleModel.on('change', onChangeCallback);
        moduleModel.on('name', onNameCallback);
        moduleModel.on('progress', onProgressCallback);
        moduleModel.on('status', onStatusCallback);

        const name = 'Super Module';
        const progress = 12345;
        const status = 'calc';

        moduleModel.set({
            name,
            progress,
            status
        });

        expect(onChangeCallback).toHaveBeenCalledTimes(1);
        expect(onNameCallback).toHaveBeenCalledTimes(1);
        expect(onProgressCallback).toHaveBeenCalledTimes(1);
        expect(onStatusCallback).toHaveBeenCalledTimes(1);
    });

    it('change events are not sent when nothing changed', () => {
        const name = 'Super Module';
        const progress = 12345;
        const status = 'calc';

        const moduleModel = new ModuleModel({
            name,
            progress,
            status
        });

        const onChangeCallback = jest.fn();
        const onNameCallback = jest.fn();
        const onProgressCallback = jest.fn();
        const onStatusCallback = jest.fn();

        moduleModel.on('change', onChangeCallback);
        moduleModel.on('name', onNameCallback);
        moduleModel.on('progress', onProgressCallback);
        moduleModel.on('status', onStatusCallback);

        moduleModel.set({
            name,
            progress,
            status
        });

        moduleModel.name = name;
        moduleModel.progress = progress;
        moduleModel.status = status;

        expect(onChangeCallback).not.toHaveBeenCalled();
        expect(onNameCallback).not.toHaveBeenCalled();
        expect(onProgressCallback).not.toHaveBeenCalled();
        expect(onStatusCallback).not.toHaveBeenCalled();
    });

    it('change events are sent when properies changed', () => {
        const moduleModel = new ModuleModel({
            id: '1',
            name: '2',
            progress: 123,
            status: 'ready'
        });

        const onChangeCallback = jest.fn();
        const onNameCallback = jest.fn();
        const onProgressCallback = jest.fn();
        const onStatusCallback = jest.fn();

        moduleModel.on('change', onChangeCallback);
        moduleModel.on('name', onNameCallback);
        moduleModel.on('progress', onProgressCallback);
        moduleModel.on('status', onStatusCallback);

        const name = 'Super Module';
        const progress = 12345;
        const status = 'calc';

        moduleModel.name = name;
        expect(onChangeCallback).toHaveBeenCalledTimes(1);
        expect(onNameCallback).toHaveBeenCalledTimes(1);
        expect(onProgressCallback).not.toHaveBeenCalled();
        expect(onStatusCallback).not.toHaveBeenCalled();
        onChangeCallback.mockClear();
        onNameCallback.mockClear();
        
        moduleModel.progress = progress;
        expect(onChangeCallback).toHaveBeenCalledTimes(1);
        expect(onProgressCallback).toHaveBeenCalledTimes(1);
        expect(onNameCallback).not.toHaveBeenCalled();
        expect(onStatusCallback).not.toHaveBeenCalled();
        onChangeCallback.mockClear();
        onProgressCallback.mockClear();
        
        moduleModel.status = status;
        expect(onChangeCallback).toHaveBeenCalledTimes(1);
        expect(onStatusCallback).toHaveBeenCalledTimes(1);
        expect(onNameCallback).not.toHaveBeenCalled();
        expect(onProgressCallback).not.toHaveBeenCalled();
    });

});
